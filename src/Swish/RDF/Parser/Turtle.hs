{-# LANGUAGE OverloadedStrings #-} -- only used in 'fromMaybe "" mbase' line of parseN3

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Turtle
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This Module implements a Turtle parser (see [1]), returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied input string, or an error indication.
--
-- REFERENCES:
--
-- 1 <http://www.w3.org/TR/turtle/>
--     Turtle, Terse RDF Triple Language
--     W3C Working Draft 09 August 2011 (<http://www.w3.org/TR/2011/WD-turtle-20110809/>)
--
-- Notes:
--
-- At present there is a lot of overlap with the N3 Parser.
--
--------------------------------------------------------------------------------

module Swish.RDF.Parser.Turtle
    ( ParseResult
    , parseTurtle      
    , parseTurtlefromText      
    {-
    , parseAnyfromText
    , parseTextFromText, parseAltFromText
    , parseNameFromText -- , parsePrefixFromText
    , parseAbsURIrefFromText, parseLexURIrefFromText, parseURIref2FromText
    -}
      
      {-
    -- * Exports for parsers that embed Turtle in a bigger syntax
    , TurtleParser, TurtleState(..), SpecialMap
    
    , getPrefix -- a combination of the old defaultPrefix and namedPrefix productions
    , n3symbol -- replacement for uriRef2 -- TODO: check this is semantically correct      
    , quickVariable -- was varid      
    , lexUriRef       
    , document, subgraph                                                   
    , newBlankNode
       -}
    )
where

import Swish.GraphClass (arc)
import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (makeNamespace, getScopeNamespace, getScopedNameURI
                       , getScopeNamespace, makeURIScopedName, makeNSScopedName)
import Swish.QName (newLName, emptyLName)

import Swish.RDF.Graph
    ( RDFGraph, RDFLabel(..)
    , ToRDFLabel(..)
    , NamespaceMap
    , addArc 
    , setNamespaces
    , emptyRDFGraph
    )

import Swish.RDF.Vocabulary
    ( LanguageTag
    , toLangTag
    , rdfType
    , rdfFirst, rdfRest, rdfNil
    , xsdBoolean, xsdInteger, xsdDecimal, xsdDouble
    , defaultBase
    )

import Swish.RDF.Datatype (makeDatatypedLiteral)

import Swish.RDF.Parser.Utils
    ( ParseResult
    , runParserWithError
    , ignore
    , noneOf
    , char
    , ichar
    , string
    , stringT
    , symbol
    , isymbol
    , lexeme
    , whiteSpace
    , hex4  
    , hex8  
    , appendURIs
    )

import Control.Applicative
import Control.Monad (foldM)

import Data.Char (ord, isAsciiLower, isAsciiUpper, isDigit) 
import Data.LookupMap (LookupMap(..), LookupEntryClass(..))
import Data.LookupMap (mapFindMaybe, mapAdd, mapReplace)
import Data.Maybe (fromMaybe, fromJust)
import Data.Word (Word32)

import Network.URI (URI(..), parseURIReference)

import Text.ParserCombinators.Poly.StateText

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | Turtle parser state
data TurtleState = TurtleState
        { graphState :: RDFGraph            -- Graph under construction
        , prefixUris :: NamespaceMap        -- namespace prefix mapping table
        , baseUri    :: URI                 -- base URI
        , nodeGen    :: Word32              -- blank node id generator
        } deriving Show

-- | Functions to update TurtleState vector (use with stUpdate)

setPrefix :: Maybe T.Text -> URI -> TurtleState -> TurtleState
setPrefix pre uri st =  st { prefixUris=p' }
    where
        p' = mapReplace (prefixUris st) (makeNamespace pre uri) 

-- | Change the base
setBase :: URI -> TurtleState -> TurtleState
setBase buri st = st { baseUri = buri }

--  Functions to access state:

-- | Return the default prefix
getDefaultPrefix :: TurtleParser Namespace
getDefaultPrefix = do
  s <- stGet
  case getPrefixURI s Nothing of
    Just uri -> return $ makeNamespace Nothing uri
    _ -> failBad "No default prefix defined; how unexpected (probably a programming error)!"

--  Map prefix to URI (naming needs a scrub here)
getPrefixURI :: TurtleState -> Maybe T.Text -> Maybe URI
getPrefixURI st pre = mapFindMaybe pre (prefixUris st)

findPrefixNamespace :: Maybe L.Text -> TurtleParser Namespace
findPrefixNamespace (Just p) = findPrefix (L.toStrict p)
findPrefixNamespace Nothing  = getDefaultPrefix

--  Return function to update graph in Turtle parser state,
--  using the supplied function of a graph
--
updateGraph :: (RDFGraph -> RDFGraph) -> TurtleState -> TurtleState
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

type TurtleParser a = Parser TurtleState a

-- | Parse as Turtle (with no real base URI).
-- 
-- See 'parseTurtle' if you need to provide a base URI.
--
parseTurtlefromText ::
  L.Text -- ^ input in N3 format.
  -> ParseResult
parseTurtlefromText = flip parseTurtle Nothing

-- | Parse a string with an optional base URI.
--            
-- Unlike 'parseN3' we treat the base URI as a URI and not
-- a QName.
--
parseTurtle ::
  L.Text -- ^ input in N3 format.
  -> Maybe URI -- ^ optional base URI
  -> ParseResult
parseTurtle txt mbase = parseAnyfromText turtleDoc mbase txt

hashURI :: URI
hashURI = fromJust $ parseURIReference "#"

emptyState :: 
  Maybe URI  -- ^ starting base for the graph
  -> TurtleState
emptyState mbase = 
  let pmap   = LookupMap [makeNamespace Nothing hashURI]
      buri   = fromMaybe (getScopedNameURI defaultBase) mbase
  in TurtleState
     { graphState = emptyRDFGraph
     , prefixUris = pmap
     , baseUri    = buri
     , nodeGen    = 0
     }
  
-- | Function to supply initial context and parse supplied term.
--
parseAnyfromText :: 
  TurtleParser a  -- ^ parser to apply
  -> Maybe URI    -- ^ base URI of the input, or @Nothing@ to use default base value
  -> L.Text       -- ^ input to be parsed
  -> Either String a
parseAnyfromText parser mbase = runParserWithError parser (emptyState mbase)

newBlankNode :: TurtleParser RDFLabel
newBlankNode = do
  n <- stQuery (succ . nodeGen)
  stUpdate $ \s -> s { nodeGen = n }
  return $ Blank (show n)
  
{-
This has been made tricky by the attempt to remove the default list
of prefixes from the starting point of a parse and the subsequent
attempt to add every new namespace we come across to the parser state.

So we add in the original default namespaces for testing, since
this routine is really for testing.

addTestPrefixes :: TurtleParser ()
addTestPrefixes = stUpdate $ \st -> st { prefixUris = LookupMap prefixTable } -- should append to existing map

-}

-- helper routines

comma, semiColon , fullStop :: TurtleParser ()
comma = isymbol ","
semiColon = isymbol ";"
fullStop = isymbol "."

sQuot, dQuot, sQuot3, dQuot3 :: TurtleParser ()
sQuot = ichar '\''
dQuot = ichar '"'
sQuot3 = ignore $ string "'''"
dQuot3 = ignore $ string "\"\"\""

match :: (Ord a) => a -> [(a,a)] -> Bool
match v = any (\(l,h) -> v >= l && v <= h)

-- a specialization of bracket
br :: String -> String -> TurtleParser a -> TurtleParser a
br lsym rsym = bracket (symbol lsym) (symbol rsym)

-- this is a lot simpler than N3
atWord :: T.Text -> TurtleParser ()
atWord s = char '@' *> lexeme (stringT s) *> pure ()

{-
Add statement to graph in the parser state; there is a special case
for the special-case literals in the grammar since we need to ensure
the necessary namespaces (in other words xsd) are added to the
namespace store.
-}

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> TurtleParser ()
addStatement s p o@(TypedLit _ dtype) | dtype `elem` [xsdBoolean, xsdInteger, xsdDecimal, xsdDouble] = do 
  ost <- stGet
  let stmt = arc s p o
      oldp = prefixUris ost
      ogs = graphState ost
      newp = mapReplace oldp (getScopeNamespace dtype)
  stUpdate $ \st -> st { prefixUris = newp, graphState = addArc stmt ogs }
addStatement s p o = stUpdate (updateGraph (addArc (arc s p o) ))

isaz, isAZ, isaZ, is09, isaZ09 :: Char -> Bool
isaz = isAsciiLower
isAZ = isAsciiUpper
isaZ c = isaz c || isAZ c
is09 = isDigit
isaZ09 c = isaZ c || is09 c

{-
Convert a string representing a double into canonical
XSD form. The input string is assumed to be syntactically
valid so we use read rather than reads. We use the String read
rather than Text one because of issues I have had in some tests
with the accuracy of the Text one.
-}
d2s :: L.Text -> RDFLabel
d2s = 
  let conv :: String -> Double
      conv = read
  in toRDFLabel . conv . L.unpack

{-
Since operatorLabel can be used to add a label with an 
unknown namespace, we need to ensure that the namespace
is added if not known. If the namespace prefix is already
in use then it is over-written (rather than add a new
prefix for the label).

TODO:
  - could we use the reverse lookupmap functionality to
    find if the given namespace URI is in the namespace
    list? If it is, use it's key otherwise do a
    mapReplace for the input namespace.
    
-}
operatorLabel :: ScopedName -> TurtleParser RDFLabel
operatorLabel snam = do
  st <- stGet
  let sns = getScopeNamespace snam
      opmap = prefixUris st
      pkey = entryKey sns
      pval = entryVal sns
      
      rval = Res snam
      
  -- the lookup and the replacement could be fused
  case mapFindMaybe pkey opmap of
    Just val | val == pval -> return rval
             | otherwise   -> do
               stUpdate $ \s -> s { prefixUris = mapReplace opmap sns }
               return rval
    
    _ -> do
      stUpdate $ \s -> s { prefixUris = mapAdd opmap sns }
      return rval
        
findPrefix :: T.Text -> TurtleParser Namespace
findPrefix pre = do
  st <- stGet
  case mapFindMaybe (Just pre) (prefixUris st) of
    Just uri -> return $ makeNamespace (Just pre) uri
    Nothing  -> failBad $ "Undefined prefix '" ++ T.unpack pre ++ ":'."

{-

Syntax productions; the Turtle NBF grammar elements are from
http://www.w3.org/TR/turtle/turtle.bnf

The element names are converted to match Haskell syntax
and idioms where possible:

  - camel Case rather than underscores and all upper case

  - upper-case identifiers prepended by _ after above form

-}

{-
[1] turtleDoc ::= (statement)*
-}
turtleDoc :: TurtleParser RDFGraph
turtleDoc = mkGr <$> (whiteSpace *> many statement *> eof *> stGet)
  where
    mkGr s = setNamespaces (prefixUris s) (graphState s)

{-
[2] statement ::= directive "." 
 | triples "."
-}
statement :: TurtleParser ()
statement = (directive <|> triples) *> fullStop

{-
[3] directive ::= prefixID 
 | base
-}
directive :: TurtleParser ()
directive = lexeme (prefixID <|> base)

{-
[4] prefixID ::= PREFIX PNAME_NS IRI_REF 
-}
prefixID :: TurtleParser ()
prefixID = do
  _prefix 
  p <- lexeme _pnameNS
  u <- _iriRef
  stUpdate (setPrefix (fmap L.toStrict p) u)

{-
[5] base ::= BASE IRI_REF 
-}
base :: TurtleParser ()
base = _base >> _iriRef >>= stUpdate . setBase

{-
[6] triples ::= subject predicateObjectList 
-}

triples :: TurtleParser ()
triples = subject >>= predicateObjectList

{-
[7] predicateObjectList ::= verb objectList ( ";" verb objectList )* (";")? 
-}

predicateObjectList :: RDFLabel -> TurtleParser ()
predicateObjectList subj = 
  let term = verb >>= objectList subj
  in sepBy1 term semiColon *> ignore (optional semiColon)
  -- in sepBy1 (lexeme term) semiColon *> ignore (optional semiColon)
  
{-
[8] objectList ::= object ( "," object )* 
-}

objectList :: RDFLabel -> RDFLabel -> TurtleParser ()
objectList subj prd = sepBy1 object comma >>= mapM_ (addStatement subj prd)

{-
[9] verb ::= predicate 
 | "a"
-}

verb :: TurtleParser RDFLabel
verb = predicate <|> (lexeme (char 'a') *> operatorLabel rdfType)
   
{-       
[10] subject ::= IRIref 
 | blank 
-}

subject :: TurtleParser RDFLabel
subject = (Res <$> iriref) <|> blank

{-
[11] predicate ::= IRIref 
-}

predicate :: TurtleParser RDFLabel
predicate = Res <$> iriref

{-
[12] object ::= IRIref 
 | blank 
 | literal
-}

object :: TurtleParser RDFLabel
object = (Res <$> iriref) <|> blank <|> literal

{-
[13] literal ::= RDFLiteral 
 | NumericLiteral 
 | BooleanLiteral 
-}

literal :: TurtleParser RDFLabel
literal = lexeme $ rdfLiteral <|> numericLiteral <|> booleanLiteral

{-
[14] blank ::= BlankNode 
 | blankNodePropertyList 
 | collection 

Since both BlankNode and blankNodePropertyList can match '[ ... ]' we pull
that out and treat this as

  blank ::= BLANK_NODE_LABEL
     | "[" (predicateObjectList | WS*) "]"
     | collection

blank :: TurtleParser RDFLabel
blank = lexeme (blankNode <|> blankNodePropertyList <|> collection)

-}

blank :: TurtleParser RDFLabel
blank = lexeme (_blankNodeLabel
                <|>
                bracket (char '[') (char ']') handleBlankNode
                <|>
                collection
                )

{-
[15] blankNodePropertyList ::= "[" predicateObjectList "]" 

We now match the brackets in the parent rule.

blankNodePropertyList :: TurtleParser RDFLabel
blankNodePropertyList = do
  bNode <- newBlankNode
  -- br "[" "]" (predicateObjectList bNode)
  bracket (satisfy (=='['))
    (satisfy (==']'))
    (_manyws *> predicateObjectList bNode *> _manyws)
  -- ignore (optional _manyws) -- TODO: this is a hack
  return bNode

-}

handleBlankNode :: TurtleParser RDFLabel
handleBlankNode = do
  bNode <- newBlankNode
  _manyws
  ignore $ optional $ predicateObjectList bNode
  _manyws
  return bNode


{-
[16] collection ::= "(" object* ")" 
-}

collection :: TurtleParser RDFLabel
collection = do
  os <- br "(" ")" (many object)
  eNode <- operatorLabel rdfNil
  case os of
    [] -> return eNode
    
    (x:xs) -> do
      sNode <- newBlankNode
      first <- operatorLabel rdfFirst
      addStatement sNode first x
      lNode <- foldM addElem sNode xs
      rest <- operatorLabel rdfRest
      addStatement lNode rest eNode
      return sNode

    where      
      addElem prevNode curElem = do
        bNode <- newBlankNode
        first <- operatorLabel rdfFirst
        rest <- operatorLabel rdfRest
        addStatement prevNode rest bNode
        addStatement bNode first curElem
        return bNode

{-
[17] <BASE> ::= "@base" 
-}

_base :: TurtleParser ()
_base = atWord "base"

{-
[18] <PREFIX> ::= "@prefix" 
-}

_prefix :: TurtleParser ()
_prefix = atWord "prefix"

{-
[19] <UCHAR> ::= ( "\\u" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] ) 
 | ( "\\U" [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] 
 [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] ) 

-}

_uchar :: TurtleParser Char
_uchar = char '\\' *> _uchar'
         
_uchar' :: TurtleParser Char
_uchar' = (char 'u' *> hex4) <|> (char 'U' *> hex8)

{-
[60s] RDFLiteral ::= String ( LANGTAG | ( "^^" IRIref ) )? 
-}

rdfLiteral :: TurtleParser RDFLabel
rdfLiteral = do
  lbl <- L.toStrict <$> turtleString
  opt <- optional ((Left <$> _langTag) <|> (string "^^" *> (Right <$> iriref)))
  return $ case opt of
             Just (Left lcode)  -> LangLit lbl lcode
             Just (Right dtype) -> TypedLit lbl dtype
             _                  -> Lit lbl

{-
[61s] NumericLiteral ::= NumericLiteralUnsigned 
 | NumericLiteralPositive 
 | NumericLiteralNegative 
-}

numericLiteral :: TurtleParser RDFLabel
numericLiteral = numericLiteralNegative <|> numericLiteralPositive <|> numericLiteralUnsigned

{-
[62s] NumericLiteralUnsigned ::= INTEGER 
 | DECIMAL 
 | DOUBLE 
-}

numericLiteralUnsigned :: TurtleParser RDFLabel
numericLiteralUnsigned = 
  d2s <$> _double
  <|> 
  (makeDatatypedLiteral xsdDecimal . L.toStrict <$> _decimal)
  <|> 
  (makeDatatypedLiteral xsdInteger . L.toStrict <$> _integer)

{-
[63s] NumericLiteralPositive ::= INTEGER_POSITIVE 
 | DECIMAL_POSITIVE 
 | DOUBLE_POSITIVE 
-}

numericLiteralPositive :: TurtleParser RDFLabel
numericLiteralPositive =
  d2s <$> _doublePositive
  <|> 
  (makeDatatypedLiteral xsdDecimal . L.toStrict <$> _decimalPositive)
  <|> 
  (makeDatatypedLiteral xsdInteger . L.toStrict <$> _integerPositive)

{-
[64s] NumericLiteralNegative ::= INTEGER_NEGATIVE 
 | DECIMAL_NEGATIVE 
 | DOUBLE_NEGATIVE 
-}

numericLiteralNegative :: TurtleParser RDFLabel
numericLiteralNegative = 
  d2s <$> _doubleNegative
  <|> 
  (makeDatatypedLiteral xsdDecimal . L.toStrict <$> _decimalNegative)
  <|> 
  (makeDatatypedLiteral xsdInteger . L.toStrict <$> _integerNegative)
   
{-
[65s] BooleanLiteral ::= "true" 
 | "false"
-}

booleanLiteral :: TurtleParser RDFLabel
booleanLiteral = makeDatatypedLiteral xsdBoolean . T.pack <$> (string "true" <|> string "false")

{-
[66s] String ::= STRING_LITERAL1 
 | STRING_LITERAL2 
 | STRING_LITERAL_LONG1 
 | STRING_LITERAL_LONG2
-}

turtleString :: TurtleParser L.Text
turtleString = 
  lexeme (
    _stringLiteralLong1 <|> _stringLiteral1 <|>
    _stringLiteralLong2 <|> _stringLiteral2)

{-
[67s] IRIref ::= IRI_REF 
 | PrefixedName 
-}

iriref :: TurtleParser ScopedName
iriref = lexeme ((makeURIScopedName <$> _iriRef) <|> prefixedName)

{-
[68s] PrefixedName ::= PNAME_LN 
 | PNAME_NS 
-}

prefixedName :: TurtleParser ScopedName
prefixedName = 
  _pnameLN <|> 
  flip makeNSScopedName emptyLName <$> (_pnameNS >>= findPrefixNamespace)

{-
[69s] BlankNode ::= BLANK_NODE_LABEL 
 | ANON 

blankNode :: TurtleParser RDFLabel
blankNode = lexeme (_blankNodeLabel <|> _anon)

-}

{-
[70s] <IRI_REF> ::= "<" ( [^<>\"{}|^`\\] - [#0000- ] | UCHAR )* ">" 

Read [#0000- ] as [#x00-#x20] from
http://lists.w3.org/Archives/Public/public-rdf-comments/2011Aug/0011.html

Unlike N3, whitespace is significant within the surrounding <>.

At present relying on Network.URI to define what characters are valid
in a URI. This is not necessarily ideal.
-}

_iriRef :: TurtleParser URI
_iriRef = do
  utxt <- bracket (char '<') (char '>') $ manySatisfy (/= '>') -- TODO: fix
  let ustr = L.unpack utxt
  case parseURIReference ustr of
    Nothing -> fail $ "Unable to convert <" ++ ustr ++ "> to a URI"
    Just uref -> do
      s <- stGet
      either fail return $ appendURIs (baseUri s) uref

{-
[71s] <PNAME_NS> ::= (PN_PREFIX)? ":" 
-}

_pnameNS :: TurtleParser (Maybe L.Text)
_pnameNS = optional _pnPrefix <* char ':'

{-
[72s] <PNAME_LN> ::= PNAME_NS PN_LOCAL 
-}

_pnameLN :: TurtleParser ScopedName
_pnameLN = do
  ns <- _pnameNS >>= findPrefixNamespace
  l <- fmap L.toStrict _pnLocal
  case newLName l of
    Just lname -> return $ makeNSScopedName ns lname
    _ -> fail $ "Invalid local name: '" ++ T.unpack l ++ "'"

{-
[73s] <BLANK_NODE_LABEL> ::= "_:" PN_LOCAL 
-}

_blankNodeLabel :: TurtleParser RDFLabel
_blankNodeLabel = (Blank . L.unpack) <$> (string "_:" *> _pnLocal)

{-

These are unused in the grammar.

[74s] <VAR1> ::= "?" VARNAME 
[75s] <VAR2> ::= "$" VARNAME 
-}

{-
[76s] <LANGTAG> ::= BASE 
 | PREFIX 
 | "@" [a-zA-Z]+ ( "-" [a-zA-Z0-9]+ )* 

I am ignoring the BASE and PREFIX lines here as they don't make sense to me.
-}

-- Note that toLangTag may fail since it does some extra
-- validation not done by the parser (mainly on the length of the
-- primary and secondary tags).
--
-- NOTE: This parser does not accept multiple secondary tags which RFC3066
-- does.
--
_langTag :: TurtleParser LanguageTag
_langTag = do
    ichar '@'
    h <- many1Satisfy isaZ
    mt <- optional (L.cons <$> char '-' <*> many1Satisfy isaZ09)
    let lbl = L.toStrict $ L.append h $ fromMaybe L.empty mt
    case toLangTag lbl of
        Just lt -> return lt
        _ -> fail ("Invalid language tag: " ++ T.unpack lbl) -- should this be failBad?
  
{-
[77s] <INTEGER> ::= [0-9]+ 
-}

_integer :: TurtleParser L.Text
_integer = many1Satisfy is09

{-
[78s] <DECIMAL> ::= [0-9]+ "." [0-9]* 
 | "." [0-9]+ 

We try to produce a canonical form for the
numbers.
-}

_decimal :: TurtleParser L.Text
_decimal = 
  let dpart = L.cons <$> char '.' <*> (fromMaybe "0" <$> optional _integer)
  in 
   (L.append <$> _integer <*> dpart)
   <|>
   (L.append "0." <$> (char '.' *> _integer))

{-
[79s] <DOUBLE> ::= [0-9]+ "." [0-9]* EXPONENT 
 | "." ( [0-9] )+ EXPONENT 
 | ( [0-9] )+ EXPONENT 

Unlike _decimal, the canonical form is enforced
later on, although it could be done here.
-}

_double :: TurtleParser L.Text
_double = 
  (L.append <$> _decimal <*> _exponent)
  <|>
  (L.append <$> _integer <*> _exponent)
  
{-
[80s] <INTEGER_POSITIVE> ::= "+" INTEGER 
[81s] <DECIMAL_POSITIVE> ::= "+" DECIMAL 
[82s] <DOUBLE_POSITIVE> ::= "+" DOUBLE 
-}

_integerPositive, _decimalPositive, _doublePositive :: TurtleParser L.Text
_integerPositive = char '+' *> _integer
_decimalPositive = char '+' *> _decimal
_doublePositive = char '+' *> _double

{-
[83s] <INTEGER_NEGATIVE> ::= "-" INTEGER 
[84s] <DECIMAL_NEGATIVE> ::= "-" DECIMAL 
[85s] <DOUBLE_NEGATIVE> ::= "-" DOUBLE 
-}

_integerNegative, _decimalNegative, _doubleNegative :: TurtleParser L.Text
_integerNegative = L.cons <$> char '-' <*> _integer
_decimalNegative = L.cons <$> char '-' <*> _decimal
_doubleNegative = L.cons <$> char '-' <*> _double

{-
[86s] <EXPONENT> ::= [eE] [+-]? [0-9]+ 
-}

_exponent :: TurtleParser L.Text
_exponent = do
  ignore $ satisfy (`elem` "eE")
  ms <- optional (satisfy (`elem` "+-"))
  e <- _integer
  case ms of
    Just '-' -> return $ L.append "E-" e
    _        -> return $ L.cons 'E' e
  
{-
[87s] <STRING_LITERAL1> ::= "'" ( ( [^'\\\n\r] ) | ECHAR | UCHAR )* "'" 
[88s] <STRING_LITERAL2> ::= '"' ( ( [^\"\\\n\r] ) | ECHAR | UCHAR )* '"' 
[89s] <STRING_LITERAL_LONG1> ::= "'''" ( ( "'" | "''" )? ( [^'\\] | ECHAR | UCHAR ) )* "'''" 
[90s] <STRING_LITERAL_LONG2> ::= '"""' ( ( '"' | '""' )? ( [^\"\\] | ECHAR | UCHAR ) )* '"""' 
-}

_stringLiteral1, _stringLiteral2 :: TurtleParser L.Text
_stringLiteral1 = _stringIt sQuot (_tChars "'\\\n\r")
_stringLiteral2 = _stringIt dQuot (_tChars "\"\\\n\r")

_stringLiteralLong1, _stringLiteralLong2 :: TurtleParser L.Text
_stringLiteralLong1 = _stringItLong sQuot3 (_tCharsLong '\'' "'\\")
_stringLiteralLong2 = _stringItLong dQuot3 (_tCharsLong '"' "\"\\")

_stringIt :: TurtleParser a -> TurtleParser Char -> TurtleParser L.Text
_stringIt sep chars = L.pack <$> bracket sep sep (many chars)

_stringItLong :: TurtleParser a -> TurtleParser L.Text -> TurtleParser L.Text
_stringItLong sep chars = L.concat <$> bracket sep sep (many chars)

_tChars :: String -> TurtleParser Char
_tChars excl = (char '\\' *> (_echar' <|> _uchar'))
               <|> noneOf excl

_tCharsLong :: Char -> String -> TurtleParser L.Text
_tCharsLong c excl = do
  mq <- optional $ oneOrTwo c
  r <- _tChars excl
  return $ L.append (fromMaybe L.empty mq) (L.singleton r)

oneOrTwo :: Char -> TurtleParser L.Text
oneOrTwo c = do
  a <- char c
  mb <- optional (char c)
  case mb of
    Just b -> return $ L.pack [a,b]
    _      -> return $ L.singleton a

{-
[91s] <ECHAR> ::= "\\" [tbnrf\\\"'] 
-}

_echar :: TurtleParser Char
_echar = char '\\' *> _echar'

_echar' :: TurtleParser Char
_echar' = 
  (char 't' *> pure '\t') <|>
  (char 'b' *> pure '\b') <|>
  (char 'n' *> pure '\n') <|>
  (char 'r' *> pure '\r') <|>
  (char '\\' *> pure '\\') <|>
  (char '"' *> pure '"') <|>
  (char '\'' *> pure '\'')


{-

Unused.

[92s] <NIL> ::= "(" (WS)* ")" 
-}

{-
[93s] <WS> ::= " " 
 | "\t" 
 | "\r" 
 | "\n"

_ws :: TurtleParser ()
_ws = ignore $ satisfy (`elem` " \t\r\n")

-}

_manyws :: TurtleParser ()
_manyws = ignore $ manySatisfy (`elem` " \t\r\n")

{-
[94s] <ANON> ::= "[" (WS)* "]" 

Unused as we do not support the use of ANON in the BlankNode
terminal.

_anon :: TurtleParser RDFLabel
_anon = br "[" "]" _manyws *> newBlankNode

-}

{-
[95s] <PN_CHARS_BASE> ::= [A-Z] 
 | [a-z] 
 | [#00C0-#00D6] 
 | [#00D8-#00F6] 
 | [#00F8-#02FF] 
 | [#0370-#037D] 
 | [#037F-#1FFF] 
 | [#200C-#200D] 
 | [#2070-#218F] 
 | [#2C00-#2FEF] 
 | [#3001-#D7FF] 
 | [#F900-#FDCF] 
 | [#FDF0-#FFFD] 
 | [#10000-#EFFFF] 
 | UCHAR 

TODO: may want to make this a Char -> Bool selector for
use with manySatisfy rather than a combinator.
-}

_pnCharsBase :: TurtleParser Char
_pnCharsBase = 
  let f c = let i = ord c
            in isaZ c || 
               match i [(0xc0, 0xd6), (0xd8, 0xf6), (0xf8, 0x2ff),
                        (0x370, 0x37d), (0x37f, 0x1fff), (0x200c, 0x200d),
                        (0x2070, 0x218f), (0x2c00, 0x2fef), (0x3001, 0xd7ff),
                        (0xf900, 0xfdcf), (0xfdf0, 0xfffd), (0x10000, 0xeffff)]
  in satisfy f <|> _uchar

{-
[96s] <PN_CHARS_U> ::= PN_CHARS_BASE 
 | "_"
-}

_pnCharsU :: TurtleParser Char
_pnCharsU = _pnCharsBase <|> char '_'

{-

Only used in VAR1/2 rules which are themselves unused.

Unused in the grammar (other than
[97s] <VARNAME> ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #00B7 | [#0300-#036F] | [# 
 203F-#2040] )* 
-}

{-
[98s] <PN_CHARS> ::= PN_CHARS_U 
 | "-" 
 | [0-9] 
 | #00B7 
 | [#0300-#036F] 
 | [#203F-#2040] 
-}

_pnChars :: TurtleParser Char
_pnChars = 
  _pnCharsU 
  <|> 
  satisfy (\c -> let i = ord c 
                 in c == '-' || isDigit c || i == 0xb7 ||
                    match i [(0x0300, 0x036f), (0x203f, 0x2040)])

{-
[99s] <PN_PREFIX> ::= PN_CHARS_BASE ( ( PN_CHARS | "." )* PN_CHARS )? 
-}

_pnPrefix :: TurtleParser L.Text
_pnPrefix = L.cons <$> _pnCharsBase <*> _pnRest
  
{-
[100s] <PN_LOCAL> ::= ( PN_CHARS_U | [0-9] ) ( ( PN_CHARS | "." )* PN_CHARS )? 
-}     

_pnLocal :: TurtleParser L.Text
_pnLocal = L.cons <$> (_pnCharsU <|> satisfy is09) 
           <*> _pnRest

{-
Extracted from PN_PREFIX and PN_LOCAL is

<PN_REST> :== ( ( PN_CHARS | "." )* PN_CHARS )?

We assume below that the match is only ever done for small strings, so
the cost of the foldr isn't likely to be large. Let's see how well
this assumption holds up.

-}

_pnRest :: TurtleParser L.Text
_pnRest = do
  lbl <- many (_pnChars <|> char '.')
  let (nret, lclean) = clean lbl
      
      -- a simple difference list implementation
      edl = id
      snocdl x xs = xs . (x:)
      appenddl = (.)
      replicatedl n x = (replicate n x ++)
  
      -- this started out as a simple automaton/transducer from
      -- http://www.haskell.org/pipermail/haskell-cafe/2011-September/095347.html
      -- but then I decided to complicate it
      -- 
      clean :: String -> (Int, String)
      clean = go 0 edl
        where
          go n acc [] = (n, acc [])
          go n acc ('.':xs) = go (n+1) acc xs 
          go 0 acc (x:xs) = go 0 (snocdl x acc) xs
          go n acc (x:xs) = go 0 (appenddl acc (snocdl x (replicatedl n '.'))) xs

  reparse $ L.replicate (fromIntegral nret) (L.singleton '.')
  return $ L.pack lclean

{-
Original from 

 chop = go 0 []
        where
        -- go :: State -> Stack -> String -> String
        go 0 _ [] = []
        go 0 _ (x:xs)
            | isSpace x = go 1 [x] xs
            | otherwise = x : go 0 xs

        go 1 ss [] = []
        go 1 ss (x:xs)
            | isSpace c = go 1 (x:ss) xs
            | otherwise = reverse ss ++ x : go 0 xs

-}

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
