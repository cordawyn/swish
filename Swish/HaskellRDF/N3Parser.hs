--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3Parser
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module implements a Notation 3 parser (see [1], [2]), returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied N3 input string, or an error indication.
--
--  Uses the Parsec monadic parser library.
--
-- REFERENCES:
--
-- 1 <http://www.w3.org/DesignIssues/Notation3.html>
--     Tim Berners-Lee's design issues series notes and description
--
-- 2 <http://www.w3.org/2000/10/swap/Primer.html>
--     Notation 3 Primer by Sean Palmer
--
--------------------------------------------------------------------------------

module Swish.HaskellRDF.N3Parser
    ( ParseResult
    , parseN3fromString
    , parseAnyfromString
    , parseTextFromString, parseAltFromString
    , parseNameFromString, parsePrefixFromString
    , parseAbsURIrefFromString, parseLexURIrefFromString, parseURIref2FromString
    -- * Exports for parsers that embed Notation3 in a bigger syntax
    , N3Parser, N3State(..), SpecialMap
    , whiteSpace, symbol, lexeme, eof, identStart, identLetter
    , defaultPrefix, namedPrefix
    , document, subgraph, uriRef2, varid, lexUriRef
    , newBlankNode
    )
where

import Swish.HaskellRDF.RDFGraph
    ( RDFGraph, RDFLabel(..)
    , NamespaceMap
    , LookupFormula(..) 
    , addArc 
    , setFormula
    , setNamespaces
    , emptyRDFGraph
    )

import Swish.HaskellRDF.GraphClass
    ( arc )

import Swish.HaskellUtils.LookupMap
    ( LookupMap(..)
    , mapFind, mapReplace, mapReplaceOrAdd )

import Swish.HaskellUtils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    , getScopePrefix 
    , getScopedNameURI
    , makeScopedName, makeUriScopedName
    , nullScopedName
    )

import Swish.HaskellRDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceRDFO
    , namespaceOWL
    , langName
    , rdf_type
    , rdf_first, rdf_rest, rdf_nil
    , owl_sameAs
    , operator_plus, operator_minus, operator_slash, operator_star
    , default_base
    )

import Swish.HaskellUtils.ErrorM
    ( ErrorM(Error,Result) )

import Network.URI (URI, isURI, isURIReference, relativeTo,
                    parseURI, parseURIReference, uriToString)

import Data.Maybe (fromJust)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)

import qualified Text.ParserCombinators.Parsec.Token as P

import Data.Char
    ( chr, digitToInt )


----------------------------------------------------------------------
--  Set up token parsers
----------------------------------------------------------------------

n3Style :: P.LanguageDef st
n3Style =
        emptyDef
            { P.commentStart   = ""
            , P.commentEnd     = ""
            , P.commentLine    = "#"
            , P.nestedComments = True
            , P.identStart     = letter <|> char '_'      -- oneOf "_"
            , P.identLetter    = alphaNum <|> char '_'
            , P.reservedNames  = []
            , P.reservedOpNames= []
            , P.caseSensitive  = True
            }

lexer :: P.TokenParser N3State
lexer = P.makeTokenParser n3Style

whiteSpace :: N3Parser ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> N3Parser String
symbol     = P.symbol     lexer

lexeme :: N3Parser a -> N3Parser a
lexeme     = P.lexeme     lexer

identStart , identLetter :: CharParser st Char
identStart  = P.identStart  n3Style
identLetter = P.identLetter n3Style

-- helper routines

istring :: String -> CharParser st ()
istring s = string s >> return ()

ichar :: Char -> CharParser st ()
ichar c = char c >> return ()

isymbol :: String -> N3Parser ()
isymbol s = symbol s >> return ()

colon :: CharParser st ()
colon = istring ":"

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | N3 parser state
data N3State = N3State
        { graphState :: RDFGraph            -- Graph under construction
        , thisNode   :: RDFLabel            -- current context node (aka 'this')
        , prefixUris :: NamespaceMap        -- namespace prefix mapping table
        , syntaxUris :: SpecialMap          -- special name mapping table
        , nodeGen    :: Int                 -- blank node id generator
        }

-- | Type for special name lookup table
type SpecialMap = LookupMap (String,ScopedName)

-- | Functions to update N3State vector (use with Parsec updateState)
setPrefix :: String -> String -> N3State -> N3State
setPrefix pre uri st =  st { prefixUris=p' }
    where
        p'    = mapReplaceOrAdd (Namespace pre uri) (prefixUris st)

-- | Set name for special syntax element
setSName :: String -> ScopedName -> N3State -> N3State
setSName nam snam st =  st { syntaxUris=s' }
    where
        s' = mapReplaceOrAdd (nam,snam) (syntaxUris st)
setSUri :: String -> String -> N3State -> N3State
setSUri nam suri = setSName nam (makeScopedName "" suri "")

-- | Get name for special syntax element, default null
getSName :: N3State -> String -> ScopedName
getSName st nam =  mapFind nullScopedName nam (syntaxUris st)

getSUri :: N3State -> String -> String
getSUri st nam = getScopedNameURI $ getSName st nam

-- | Lookup prefix in table and return URI or 'prefix:'
mapPrefix :: NamespaceMap -> String -> String
mapPrefix ps pre = mapFind (pre++":") pre ps

--  Functions to access state:
--  Map prefix to namespace
getPrefixNs :: N3State -> String -> Namespace
getPrefixNs st pre = Namespace pre (mapPrefix (prefixUris st) pre)

--  Map ScopedName using prefix table
--  (Ignore URI in supplied ScopedName)
getPrefixScopedName :: N3State -> ScopedName -> ScopedName
getPrefixScopedName st snam = ScopedName (getPrefixNs st pre) loc
    where
        pre = getScopePrefix snam
        loc = snLocal snam

--  Return function to update graph in N3 parser state,
--  using the supplied function of a graph
--  (use returned function with Parsec updateState)
updateGraph :: ( RDFGraph -> RDFGraph ) -> ( N3State -> N3State )
updateGraph f s = s { graphState = f (graphState s) }

--  Define default table of namespaces
prefixTable :: [Namespace]
prefixTable =   [ namespaceRDF
                , namespaceRDFS
                , namespaceRDFD     -- datatypes
                , namespaceRDFO     -- operators
                , namespaceOWL
                ]

--  Define default special-URI table
specialTable :: [(String,ScopedName)]
specialTable =  [ ( "a",         rdf_type       ),
                  ( "equals",    owl_sameAs     ),
                  ( "listfirst", rdf_first      ),
                  ( "listrest",  rdf_rest       ),
                  ( "listnull",  rdf_nil        ),
                  ( "plus",      operator_plus  ),
                  ( "minus",     operator_minus ),
                  ( "slash",     operator_slash ),
                  ( "star",      operator_star  ),
                  ( "base",      default_base   ) ]

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

type N3Parser a = GenParser Char N3State a

type ParseResult = ErrorM RDFGraph -- PResult RDFGraph | PError String

parseN3fromString :: String -> ParseResult
parseN3fromString input =
        case parseAnyfromString document Nothing input of
            Left  err -> Error err
            Right gr  -> Result gr

-- | Function to supply initial context and parse supplied term
--
parseAnyfromString :: N3Parser a      -- ^ parser to apply
                      -> Maybe String -- ^ base URI of the input, or @Nothing@ to use default base value
                      -> String       -- ^ input to be parsed
                      -> Either String a
parseAnyfromString parser base input =
        let
            pmap   = LookupMap prefixTable
            smap   = LookupMap specialTable
            bmap   = case base of
                Nothing -> smap
                Just bs -> mapReplace smap ("base",makeUriScopedName bs)
            pstate = N3State
                    { graphState = emptyRDFGraph
                    , thisNode   = NoNode
                    , prefixUris = pmap
                    , syntaxUris = bmap
                    , nodeGen    = 0
                    }
            result = runParser parser pstate "" input
        in
            case result of
                Left  err -> Left  (show err)
                Right res -> Right res

newBlankNode :: N3Parser RDFLabel
newBlankNode =
        do  { s <- getState
            ; let n = nodeGen s + 1
            ; setState ( s { nodeGen = n } )
            ; return (Blank (show n))
            }

--  Test functions for selected element parsing

parseTextFromString :: String -> String -> Either String String
parseTextFromString s =
    parseAnyfromString (string s) Nothing

parseAltFromString :: String -> String -> String -> Either String String
parseAltFromString s1 s2 =
    parseAnyfromString ( string s1 <|> string s2 ) Nothing

parseNameFromString :: String -> Either String String
parseNameFromString =
    parseAnyfromString name Nothing

parsePrefixFromString :: String -> Either String Namespace
parsePrefixFromString =
    parseAnyfromString prefix Nothing

parseAbsURIrefFromString :: String -> Either String String
parseAbsURIrefFromString =
    parseAnyfromString absUriRef Nothing

parseLexURIrefFromString :: String -> Either String String
parseLexURIrefFromString =
    parseAnyfromString lexUriRef Nothing

parseURIref2FromString :: String -> Either String ScopedName
parseURIref2FromString =
    parseAnyfromString uriRef2 Nothing

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

--  document         = directive* statement-list

document :: N3Parser RDFGraph
document =
        do  { whiteSpace
            ; _ <- many directive
            ; statements
            ; eof
            ; s <- getState
            ; return $ setNamespaces (prefixUris s) (graphState s)
            }


--  directive        = "@prefix" prefix ":" uriRef2 "."   // Namespace declaration
--                   | "@prefix" ":" uriRef2 "."          // Default namespace
--                   | "@equivalence" uriRef2 "."         // Alternative to daml:equivalent
--                   | "@listfirst" uriRef2 "."           // Alternative to n3:first
--                   | "@listrest"  uriRef2 "."           // Alternative to n3:rest
--                   | "@listnull"  uriRef2 "."           // Alternative to n3:null
--                   | "@plus"  uriRef2 "."               // Alternative to operator:plus
--                   | "@minus" uriRef2 "."               // Alternative to operator:minus
--                   | "@slash" uriRef2 "."               // Alternative to operator:slash
--                   | "@star"  uriRef2 "."               // Alternative to operator:star
--                   | "@base"  uriRef2 "."               // Base URI for relative URIs.

directive :: N3Parser ()
directive =
        do  { try $ isymbol "@prefix"
            ; ( defaultPrefix <|> namedPrefix )
            }
    <|>
        (istring "@" >> syntaxUri)
    <?>
        "directive"

defaultPrefix :: N3Parser ()
defaultPrefix =
        do  { isymbol ":"
            ; u <- uriRef2
            ; isymbol "."
            ; updateState $ setPrefix "" (getScopedNameURI u)
            }

namedPrefix :: N3Parser ()
namedPrefix =
        do  { n <- name
            ; isymbol ":"
            ; u <- uriRef2
            ; isymbol "."
            ; updateState $ setPrefix n (getScopedNameURI u)
            }

syntaxUri :: N3Parser ()
syntaxUri =
        do  { s <- uriName
            ; u <- uriRef2
            ; isymbol "."
            ; updateState $ setSUri s (getScopedNameURI u)
            }

uriName :: N3Parser String
uriName =
        try (symbol "equivalence")
    <|> try (symbol "listfirst")
    <|> try (symbol "listrest")
    <|> try (symbol "listnull")
    <|> try (symbol "plus")
    <|> try (symbol "minus")
    <|> try (symbol "slash")
    <|> try (symbol "star")
    <|> try (symbol "base")
    <?> "special URI directive"


--  statements       = [ statement ( "." statement )* ]
--
--  statement        = subject property-list
--
--  properties       = [ property ( ";" property )* ]
--
--  New statements are added to the user state graph

statements :: N3Parser ()
statements = sepEndBy1 statement (symbol ".") >> return ()

statement :: N3Parser ()
statement = subject >>= optional . properties

properties :: RDFLabel -> N3Parser ()
properties subj = sepBy1 (property subj) (symbol ";") >> return ()


--  property         = verb object-list
--                   | ":-" anon-node           // Creates anon-node aongside the current node
--  verb             = ">-" prop "->"           // has 'prop' of
--                   | "<-" prop "<-"           // is  'prop' of
--                   | operator                 // has operator:'operator' of (???)
--                   | prop                     // has 'prop' of -- shorthand
--                   | "has" prop "of"          // has 'prop' of
--                   | "is" prop "of"           // is  'prop' of
--                   | "a"                      // has rdf:type of
--                   | "="                      // has daml:equivalent of
--
--  subj    is the subject node for these properties.
--
--  New statements are added to the graph in the parser's user state.

property :: RDFLabel -> N3Parser ()
property subj =
    do  { (ppty,swap) <- verb
        ; objects subj ppty swap
        }
    <|>
    (isymbol ":-" >> anonNode subj >> return ())

verb :: N3Parser (RDFLabel,Bool)
verb =  do  { p <- prop ;                                return (p,False) }
    <|> do  { p <- operator ;                            return (p,False) }
    <|> do  { isymbol ">-"  ; p <- prop ; isymbol "->" ; return (p,False) }
    <|> do  { isymbol "<-"  ; p <- prop ; isymbol "<-" ; return (p,True) }
    <|> do  { isymbol "has" ; p <- prop ; isymbol "of" ; return (p,False) }
    <|> do  { isymbol "is"  ; p <- prop ; isymbol "of" ; return (p,True) }
    <|> do  { isymbol "a"
            ; lab <- operatorLabel rdf_type
            ; return (lab,False)
            }
    <|> do  { isymbol "="
            ; lab <- operatorLabel owl_sameAs
            ; return (lab,False)
            }
    <?> "property"


--  objects          = object
--                   | object "," object-list
--
--  subj    is the subject node for the new statements,
--  prop    is the property node for the new statements.
--  swap    is true if the subject/object values in the resulting statement
--          are to be swapped (for "is <prop> of", etc.)
--
--  New statements are added to the graph in the parser's user state

objects :: RDFLabel -> RDFLabel -> Bool -> N3Parser ()
objects subj ppty swap =
        do  { _ <- sepBy1 (object subj ppty swap) (symbol ",")
            ; return ()
            }

--  anonNode         = "[" property-list "]"    // Something with given properties
--                   | "{" statement-list "}"   // List of statements as resource
--                   | "(" node-list ")"        // Construct list with
--                                              //   rdf:first, rdf:rest, rdf:nil
--
--  subj    is the subject node with which the new anonymous node is equated,
--
--  The anonymous node value is returned by this parser (which is often the same
--  as the supplied subject node, but not always).
--
--  New statements are added to the graph in the parser's user state
--  (in the case of a formula, a new graph and parser are created, and
--  the graph arcs are added to this new graph).

anonNode :: RDFLabel -> N3Parser RDFLabel
anonNode subj =
        do  { isymbol "[" ; properties subj ; isymbol "]" ; return subj }
    <|> do  { isymbol "{" ; form <- formula  subj ; isymbol "}" ; return form }
    <|> do  { isymbol "(" ; list <- nodeList subj ; isymbol ")" ; return list }
    <?> "anon node (\"[\", \"(\" or \"{\")"

--  This method allows a statement list to be parsed as a subgraph
--  whose value is associated with the supplied node of the current
--  graph.

formula :: RDFLabel -> N3Parser RDFLabel
formula subj =
        do  { subgr <- subgraph subj
            ; updateState $ updateGraph
                          $ setFormula (Formula subj subgr)
            ; return subj
            }

subgraph :: RDFLabel -> N3Parser RDFGraph
subgraph this =
        do  { pstate <- getState
            ; let fstate = pstate { graphState = emptyRDFGraph, thisNode = this }
            ; setState fstate       -- switch new state into parser
            ; statements            -- parse statements of formula
            ; fstate' <- getState
            ; let nstate = pstate { nodeGen = nodeGen fstate' }
            ; setState nstate       -- swap back state, with updated nodeGen
            ; return (graphState fstate')
            }

--  prop             = uri-ref2
--                   | varid
--
--  Returns URI value as a Node

prop :: N3Parser RDFLabel
prop = nodeid <|> varid <|> uriNode


--  operator         = "+"                      // >- operator:plus ->
--                   | "-"                      // >- operator:minus ->
--                   | "/"                      // >- operator:slash ->
--                   | "*"                      // >- operator:star->
--
--  If matched, the operator is returned as a node value.

operator :: N3Parser RDFLabel
operator =
        do  { isymbol "+" ; operatorLabel operator_plus  }
    <|> do  { isymbol "-" ; operatorLabel operator_minus }
    <|> do  { isymbol "*" ; operatorLabel operator_star  }
    <|> do  { isymbol "/" ; operatorLabel operator_slash }
    <?> ""

operatorLabel :: ScopedName -> N3Parser RDFLabel
operatorLabel snam =
        do  { s <- getState
            ; return $ Res (getPrefixScopedName s snam)
            }

--  subject          = node

subject :: N3Parser RDFLabel
subject = node


--  object           = litNode
--
--  This production adds a new triple to the graph state,
--  using the supplied subject and property values.
--  If swap is True, the subject and object positions are
--  swapped.

object :: RDFLabel -> RDFLabel -> Bool -> N3Parser ()
object subj ppty True = do
  o <- litNode
  addStatement o ppty subj

object subj ppty _ = do
  o <- litNode
  addStatement subj ppty o
        
-- Add statement to graph in N3 parser state

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> N3Parser ()
addStatement s p o = updateState (updateGraph (addArc (arc s p o) ))


--  nodeList         = litNode*
--
--  subj    is the node from which the list is linked.
--
--  Returns the supplied head of list or Nil node allocated.
--
--  Link first element of link to list head, scan rest of list,
--  and return the list head;  otherwise return a node rdf_null.
--
--  This slightly convoluted pattern is to deal with two different
--  occurrences of a list node:
--    <node> :- ( l1, l2, ... )
--      Here, <node> (the supplied subj) is the listhead.
--    <node> prop ( l1, l2, ... )
--      Here, the a new blank is supplied as subj to be the listhead.
--  In either case, if the list is non-empty, the supplied subj
--  is returned.  But if the list is empty, a rdf_null node is returned.
--  In the second case, the invoking production must use the returned
--  value.

nodeList :: RDFLabel -> N3Parser RDFLabel
nodeList subj =
        do  { val   <- litNode
            ; first <- operatorLabel rdf_first
            ; addStatement subj first val
            ; nodeList1 subj
            ; return subj
            }
    <|> operatorLabel rdf_nil
    <?> "Node or ')'"

nodeList1 :: RDFLabel -> N3Parser ()
nodeList1 prev =
        do  { val   <- litNode
            ; lnk   <- newBlankNode
            ; first <- operatorLabel rdf_first
            ; rest  <- operatorLabel rdf_rest
            ; addStatement lnk  first val
            ; addStatement prev rest  lnk
            ; nodeList1 lnk
            }
    <|> do  { nil   <- operatorLabel rdf_nil
            ; rest  <- operatorLabel rdf_rest
            ; addStatement prev rest nil
            }
    <?> "Node or ')'"


--  lit-node         = node
--                   | str-node [ "@" lang ] [ "^^" uriRef2 ]
--  str-node         = '"' constant-value '"'
--                   | '"""' constant value '"""'   // Including single or double occurences of
--                                                  //   quotes and/or newlines
--
--  Returns a new node value.

litNode :: N3Parser RDFLabel
litNode =
        node
    <|> do  { s <- strNode
            ; t <- litTypeOrLang
            ; return $ Lit s t
            }
    <?> "URI, blank node or literal"

strNode :: N3Parser String
strNode =
        tripleQuoteString
    <|> singleQuoteString


litTypeOrLang :: N3Parser (Maybe ScopedName)
litTypeOrLang =
        langTag
    <|> typeUri
    <|> return Nothing
    <?> "'@tag' (language tag) or '^^name' (datatype URI)"

langTag :: N3Parser (Maybe ScopedName)
langTag =
        do  { istring "@"
            ; l <- name -- name1 letter
            ; return $ Just (langName l)
            }
    <?> "'@tag' (language tag)"

typeUri :: N3Parser (Maybe ScopedName)
typeUri =
        do  { istring "^^"
            ; u <- uriRef2
            ; return $ Just u
            }
    <?> "'^^name' (datatype URI)"

--  node             = nodeid
--                   | varid
--                   | uri-ref2
--                   | anon-node
--
--  nodeid           = "_:" name
--
--  varid            = "?" name
--
--  Returns a new node value.

node :: N3Parser RDFLabel
node =  nodeid
    <|> varid
    <|> uriNode
    <|> do  { n <- newBlankNode
            ; anonNode n
            }
    <?> "URI or blank node"

--  Identified blank node in input
--
--  Note that automatically generated blank node identifiers start with
--  a digit, where input node identifiers start with a letter, so there
--  can be no clash.  Care is needed when serializing a graph to ensure
--  that future clashes are avoided.

nodeid :: N3Parser RDFLabel
-- nodeid = lexeme nodeid1
nodeid =
    do  { istring "_:"
        ; n <- name
        ; return (Blank n)
        }

--  variable identifier

varid :: N3Parser RDFLabel
varid = do  { istring "?"
            ; n <- name
            ; return (Var n)
            }

--  uriNode          = qname
--                   | "<" URI-reference ">"
--                   | "this"

uriNode :: N3Parser RDFLabel
uriNode =
        do  { sn <- uriRef2 ;
            ; return ( Res sn )
            }
    <|>
        do  { istring "this"
            ; s <- getState
            ; return ( thisNode s )
            }
    <?> "URI node"


--  uriRef2          = qname
--                   | ":" local-name
--                   | "<" URI-reference ">"
--  qname            = prefix ":" local-name
--
--  prefix           = name                         // Namespace prefix
--
--  local-name       = name                         // Local name (namespace qualified)
--
--  name             = alpha alphanumeric*
--
--  alpha            = "a"-"z"
--                   | "A"-"Z"
--                   | "_"
--
--  alphanumeric     = alpha
--                   | "0"-"9"
--
--  URI-reference    = (conforming to syntax in RFC2396)
--
--  uriRef2 returns a ScopedName.

uriRef2 :: N3Parser ScopedName
uriRef2 = lexeme (try uriRef2a)
    <?> "URI or QName"

uriRef2a :: N3Parser ScopedName
uriRef2a =
        do  { ns    <- prefix
            ; colon
            ; local <- localname
            ; return $ ScopedName ns local
            }
    <|>
        do  { colon
            ; ns    <- defaultprefix
            ; local <- localname
            ; return $ ScopedName ns local
            }
    <|>
        do  { u <- absUriRef
            ; return $ makeUriScopedName u
            }
    <?> "URI or QName"

prefix :: N3Parser Namespace
prefix =
        do  { pref <- prefixname
            ; st   <- getState
            ; return (getPrefixNs st pref)   -- map prefix to namespace
            }

defaultprefix :: N3Parser Namespace
defaultprefix =
        do  { st <- getState
            ; return (getPrefixNs st "")
            }

name :: N3Parser String
name =  lexeme $ name1 identStart

prefixname :: N3Parser String
prefixname =  name1 identStart

localname :: N3Parser String
localname =  lexeme $ name1 identLetter

--  'name1' is a name without following whitespace
--  initChar is a parser for the first character
name1 :: N3Parser Char -> N3Parser String
name1 initChar =
    do  { c  <- initChar
        ; cs <- many identLetter
        ; return (c:cs)
        }
    <?> "identifier"


----------------------------------------------------------------------
-- Lexical support
----------------------------------------------------------------------
--
-- The following code adapted from ParsecToken,
-- modified to handle different escape conventions and triple-quoted strings
--      \c
--      \uhhhh
--      \Uhhhhhhhh
--
-- Regular single-quoted string -- cannot be split over line breaks

singleQuoteString :: N3Parser String
singleQuoteString =
    lexeme
    (   between (char '"') (char '"' <?> "end of string (\")") anyStringChars
    <?> "literal string" )

anyStringChars :: CharParser st String
anyStringChars =
        do  { str <- many stringChar
            ; return (foldr (maybe id (:)) "" str)
            }

-- Triple-quoted string -- may include line breaks, '"' or '""'.
tripleQuoteString :: N3Parser String
tripleQuoteString =
    lexeme
    (   do  { str <- between (try $ string "\"\"\"")
                             (string "\"\"\"" <?> "end of string (\"\"\")")
                             (many tripleQuoteSubstring)
            ; return (foldr (++) "" str)
            }
    <?> "triple-quoted literal string" )

-- Match non-quote substring or one or two quote characters
tripleQuoteSubstring :: N3Parser String
tripleQuoteSubstring =
        tripleQuoteSubstring1
    <|> try sqTripleQuoteSubstring1
    <|> try dqTripleQuoteSubstring1

dqTripleQuoteSubstring1 :: N3Parser String
dqTripleQuoteSubstring1 =
        do  { istring "\"\""
            ; s <- tripleQuoteSubstring1
            ; return $ "\"\""++s
            }

sqTripleQuoteSubstring1 :: N3Parser String
sqTripleQuoteSubstring1 =
        do  { ichar '"'
            ; s <- tripleQuoteSubstring1
            ; return $ '"' : s
            }

-- match at least one non-quote character in a triple-quoted string
tripleQuoteSubstring1 :: N3Parser String
tripleQuoteSubstring1 =
        do  { str <- many1 tripleQuoteStringChar
            ; return $ foldr (maybe id (:)) "" str
            }

tripleQuoteStringChar :: CharParser st (Maybe Char)
tripleQuoteStringChar =
            stringChar
    <|> do  { istring "\n"
            ; return $ Just '\n'
            }

stringChar :: CharParser st (Maybe Char)
stringChar      =
        do  { c <- stringLetter
            ; return $ Just c
            }
    <|> stringEscape
    <?> "string character"

stringLetter :: CharParser st Char
stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c >= '\032'))

stringEscape :: CharParser st (Maybe Char)
stringEscape    =
        do  { ichar '\\'
            ; do { esc <- escapeCode; return (Just esc) }
            }

-- escape codes
escapeCode :: CharParser st Char
escapeCode      = charEsc <|> charUCS2 <|> charUCS4 <?> "escape code"

-- \c
charEsc :: CharParser st Char
charEsc         = choice (map parseEsc escMap)
                where
                    parseEsc (c,code) = do { ichar c; return code }
                    escMap            = zip "nrt\\\"\'" "\n\r\t\\\"\'"

-- \uhhhh
charUCS2 :: CharParser st Char
charUCS2        =
        do  { ichar 'u'
            ; n <- numberFW 16 hexDigit 4 0
            ; return $ chr n
            }

-- \Uhhhhhhhh
charUCS4 :: CharParser st Char
charUCS4        =
        do  { ichar 'U'
            ; n <- numberFW 16 hexDigit 8 0
            ; return $ chr n
            }

-- parse fixed-width number:
numberFW :: Int -> CharParser st Char -> Int -> Int -> CharParser st Int
numberFW _    _         0     val = return val
numberFW base baseDigit width val =
        do  { d <- baseDigit
            ; numberFW base baseDigit (width-1) (val*base + digitToInt d)
            }


----------------------------------------------------------------------
--  Parse a URI reference from the input
--  The result returned has absolute form;  relative URIs are resolved
--  relative to the current base prefix (set using "@base").
--
--  [[[TODO:  rework the URI parser to use the Parsec library]]]

--  lexeme version
lexUriRef :: N3Parser String
lexUriRef = lexeme absUriRef

-- from Swish.HaskellUtils.ProcessURI
absoluteUriPart :: String -- ^ URI base
                   -> String -- ^ URI reference
                   -> String
absoluteUriPart base rel = showURI $ fromJust $ relativeTo (fromJust (parseURIReference rel)) (fromJust (parseURI base))
  
showURI :: URI -> String
showURI u = uriToString id u ""

absUriRef :: N3Parser String
absUriRef =
        do  { u <- between (char '<') (char '>' <?> "end of URI '>'") anyUriChars
            ; if isURI u
              then return u
              else
              if isURIReference u
              then
                do  { s <- getState
                    ; (return $ absoluteUriPart (getSUri s "base") u)
                    }
              else fail ("Invalid URI: <"++u++">")
            }

anyUriChars :: N3Parser String
anyUriChars = many uriChar

uriChar :: N3Parser Char
uriChar =
        alphaNum
    <|> oneOf "[];?:@&=+$,-_.!~*'()%//#"
    <?> "URI character"


--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
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
