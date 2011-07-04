--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  NTParser
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This Module implements a NTriples parser (see [1]), returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied NTriples input string, or an error indication.
--
-- REFERENCES:
--
-- 1 <http://www.w3.org/TR/rdf-testcases/#ntriples>
--     RDF Test Cases
--     W3C Recommendation 10 February 2004
--
--------------------------------------------------------------------------------

module Swish.RDF.NTParser
    ( ParseResult
    , parseNT      
    , parsefromString
    
      {-
    -- * Exports for parsers that embed NTriples in a bigger syntax
    , NTParser, NTState(..)
    , ntripleDoc
    , line, ws, comment, eoln
    , character, name, triple
    , subject, predicate, object
    , uriref, urirefLbl
    , nodeID, literal, language
      -}
      
    )
where

import Swish.RDF.RDFGraph
    ( RDFGraph, RDFLabel(..)
    , addArc 
    , emptyRDFGraph
    )

import Swish.RDF.GraphClass (arc)

import Swish.Utils.Namespace (ScopedName(..), makeUriScopedName)

import Swish.RDF.Vocabulary (langName)

import Swish.RDF.RDFParser ( ParseResult
    , ignore
    , skipMany
    , noneOf
    , char
    , string
    , eoln
    , fullStop
    , hex4
    , hex8
    )
  
{-
import Swish.RDF.RDFParser
    ( ParseResult, RDFParser
    , ignore
    , annotateParsecError
    )
-}

import Control.Applicative
import Control.Monad (when)

import Network.URI (parseURI)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Char (chr) 
import Data.Maybe (fromMaybe, isNothing)

import Text.ParserCombinators.Poly.StateText

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | NT parser state
data NTState = NTState
        { graphState :: RDFGraph            -- Graph under construction
        }

--  Return function to update graph in NT parser state,
--  using the supplied function of a graph. This is for use
--  with stUpdate.
--
updateGraph :: (RDFGraph -> RDFGraph) -> NTState -> NTState
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

type NTParser a = Parser NTState a

-- | Parse a string.
-- 
parseNT ::
  L.Text -- ^ input in NTriples format.
  -> ParseResult
parseNT = parsefromText ntripleDoc

{-
-- useful for testing
test :: String -> RDFGraph
test = either error id . parseNT
-}

-- | Function to supply initial context and parse supplied term.
--
-- We augment the Parsec error with the context.
--
parsefromString :: 
    NTParser a      -- ^ parser to apply
    -> String       -- ^ input to be parsed
    -> Either String a
parsefromString parser = parsefromText parser . L.pack

-- | Function to supply initial context and parse supplied term.
--
parsefromText :: 
    NTParser a      -- ^ parser to apply
    -> L.Text       -- ^ input to be parsed
    -> Either String a
parsefromText parser input =
        let istate = NTState
                    { graphState = emptyRDFGraph
                    }
            (result, _, _) = runParser parser istate input
        in result 
           
-- helper routines

{-
lineFeed :: NTParser ()
lineFeed = ignore (char '\r')
-}

-- Add statement to graph in NT parser state

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> NTParser ()
addStatement s p o = stUpdate (updateGraph (addArc (arc s p o) ))

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

{-

EBNF from the specification, using the notation from XML 1.0, second edition,
is included inline below.

We do not force ASCII 7-bit semantics here yet.

space	::=	#x20 /* US-ASCII space - decimal 32 */	
cr	::=	#xD /* US-ASCII carriage return - decimal 13 */	
lf	::=	#xA /* US-ASCII line feed - decimal 10 */	
tab	::=	#x9 /* US-ASCII horizontal tab - decimal 9 */	

The productions are kept as close as possible to the specification
for now.

-}

{-
ntripleDoc	::=	line*	
line	::=	ws* ( comment | triple )? eoln	

We relax the rule that the input must be empty or end with a new line.

ntripleDoc :: NTParser RDFGraph
ntripleDoc = graphState <$> (many line *> eof *> getState)

line :: NTParser ()
line = skipMany ws *> optional (comment <|> triple) *> eoln
-}

ntripleDoc :: NTParser RDFGraph
ntripleDoc = graphState <$> (sepBy line eoln *> optional eoln *> skipWS *> eof *> stGet)

line :: NTParser ()
line = skipWS *> ignore (optional (comment <|> triple))

{-
ws	::=	space | tab	

Could use whiteSpace rule here, but that would permit
constructs (e.g. comments) where we do not support them.
-}

isWS :: Char -> Bool
isWS = (`elem` " \t")

{-
ws :: NTParser ()
-- ws = ignore (char ' ' <|> tab)
ws = ignore $ satisfy isWS
-}
           
skipWS :: NTParser ()
skipWS = ignore $ manySatisfy isWS

skip1WS :: NTParser ()
skip1WS = ignore $ many1Satisfy isWS

{-
comment	::=	'#' ( character - ( cr | lf ) )*	
-}

comment :: NTParser ()
comment = char '#' *> skipMany (noneOf "\r\n")

{-
eoln	::=	cr | lf | cr lf	
-}

{-
name	::=	[A-Za-z][A-Za-z0-9]*	
-}

hChars, bChars :: String
hChars = ['a'..'z'] ++ ['A'..'Z']
bChars = hChars ++ ['0'..'9']

-- cons is not particularly efficient
name :: NTParser String
-- name = (:) <$> satisfy (`elem` hChars) <*> manySatisfy (`elem` bChars)
name = L.unpack <$> (L.cons <$> satisfy (`elem` hChars) <*> manySatisfy (`elem` bChars))

{-
triple	::=	subject ws+ predicate ws+ object ws* '.' ws*	

-}

triple :: NTParser ()
triple = 
  {- tryin to be fancy but addStatement is a Parser not a pure function
  addStatement 
  <$> (subject <* skip1WS)
  <*> (predicate <* skip1WS)
  <*> (object <* (skipWS *> fullStop *> skipWS))
  -}
  
  do
    s <- subject
    skip1WS
    p <- predicate
    skip1WS
    o <- object
    skipWS
    fullStop
    skipWS
    addStatement s p o

{-
subject	::=	uriref | nodeID	
predicate	::=	uriref	
object	::=	uriref | nodeID | literal	
-}

subject :: NTParser RDFLabel
subject = urirefLbl <|> nodeID

predicate :: NTParser RDFLabel
predicate = urirefLbl

object :: NTParser RDFLabel
object = urirefLbl <|> nodeID <|> literal

{-
uriref	::=	'<' absoluteURI '>'	
absoluteURI	::=	character+ with escapes as defined in section URI References	

-}

uriref :: NTParser ScopedName
uriref = do
  -- not ideal, as want to reject invalid characters immediately rather than via parseURI
  ustr <- L.unpack <$> bracket (char '<') (char '>') (many1Satisfy (/= '>'))
  -- ustr <- bracket (char '<') (char '>') $ many1 character -- looks like need to exclude > from character
  -- ustr <- char '<' *> manyTill character (char '>')
  when (isNothing (parseURI ustr)) $
    failBad ("Invalid URI: <" ++ ustr ++ ">")
  return $ makeUriScopedName ustr

urirefLbl :: NTParser RDFLabel
urirefLbl = Res <$> uriref

{-
nodeID	::=	'_:' name	
-}

nodeID :: NTParser RDFLabel
nodeID = Blank <$> (string "_:" *> name)

{-  
literal	::=	langString | datatypeString	
langString	::=	'"' string '"' ( '@' language )?	
datatypeString	::=	'"' string '"' '^^' uriref	
language	::=	[a-z]+ ('-' [a-z0-9]+ )*
encoding a language tag.	
string	::=	character* with escapes as defined in section Strings	

-}

literal :: NTParser RDFLabel
literal = Lit <$> (T.pack <$> ntstring) <*> optional dtlang

ntstring :: NTParser String
ntstring = bracket (char '"') (char '"') (many character)

dtlang :: NTParser ScopedName
dtlang = 
    (char '@' *> language)
    <|> (string "^^" *> uriref)

language :: NTParser ScopedName
language = do
  h <- many1Satisfy (`elem` ['a'..'z'])
  mt <- optional ( L.cons <$> char '-' <*> many1Satisfy (`elem` (['a'..'z'] ++ ['0'..'9'])) )
  return $ langName $ L.unpack $ L.append h $ fromMaybe L.empty mt

{-
String handling: 

EBNF has:

character	::=	[#x20-#x7E] /* US-ASCII space to decimal 126 */	

Additional information from:

  http://www.w3.org/TR/rdf-testcases/#ntrip_strings

N-Triples strings are sequences of US-ASCII character productions encoding [UNICODE] character strings. The characters outside the US-ASCII range and some other specific characters are made available by \-escape sequences as follows:

 Unicode character
 (with code point u)	N-Triples encoding
 [#x0-#x8]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 #x9	\t
 #xA	\n
 [#xB-#xC]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 #xD	\r
 [#xE-#x1F]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 [#x20-#x21]	the character u
 #x22	\"
 [#x23-#x5B]	the character u
 #x5C	\\
 [#x5D-#x7E]	the character u
 [#x7F-#xFFFF]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 [#10000-#x10FFFF]	\UHHHHHHHH
 8 required hexadecimal digits HHHHHHHH encoding Unicode character u
 where H is a hexadecimal digit: [#x30-#x39],[#x41-#x46] (0-9, uppercase A-F).

This escaping satisfies the [CHARMOD] section Reference Processing Model on making the full Unicode character range U+0 to U+10FFFF available to applications and providing only one way to escape any character.

-}

{-
asciiChars :: String
asciiChars = map chr [0x20..0x7e]

asciiCharsNT :: String
asciiCharsNT = filter (`notElem` "\\\"") asciiChars
-}

-- 0x22 is " and 0x5c is \
asciiChars :: String
asciiChars = map chr $ 0x20 : 0x21 : [0x23..0x5b] ++ [0x5d..0x7e]

protectedChar :: NTParser Char
protectedChar =
  (char 't' *> return '\t')
  <|> (char 'n' *> return '\n')
  <|> (char 'r' *> return '\r')
  <|> (char '"' *> return '"')
  <|> (char '\\' *> return '\\')
  <|> (char 'u' *> hex4)
  <|> (char 'U' *> hex8)

character :: NTParser Char
character = 
  (char '\\' *> protectedChar)
  <|> satisfy (`elem` asciiChars)

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
