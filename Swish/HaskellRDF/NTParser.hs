--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  NTParser
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module implements a NTriples parser (see [1]), returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied NTriples input string, or an error indication.
--
--  Uses the Parsec monadic parser library.
--
-- REFERENCES:
--
-- 1 <http://www.w3.org/TR/rdf-testcases/#ntriples>
--     RDF Test Cases
--     W3C Recommendation 10 February 2004
--
--------------------------------------------------------------------------------

module Swish.HaskellRDF.NTParser
    ( ParseResult
    , parseNT      
    , parsefromString
    
    -- * Exports for parsers that embed NTriples in a bigger syntax
    , NTParser, NTState(..)
    , ntripleDoc
    , line, ws, comment, eoln
    , character, name, triple
    , subject, predicate, object
    , uriref, urirefLbl
    , nodeID, literal, language

    )
where

import Swish.HaskellRDF.RDFGraph
    ( RDFGraph, RDFLabel(..)
    , addArc 
    , emptyRDFGraph
    )

import Swish.HaskellRDF.GraphClass
    ( arc )

import Swish.HaskellUtils.Namespace
    ( ScopedName(..)
    , makeUriScopedName
    )

import Swish.HaskellRDF.Vocabulary (langName)

import Swish.HaskellRDF.RDFParser
    ( ParseResult, RDFParser
    , annotateParsecError
    )

import Swish.HaskellUtils.ErrorM
    ( ErrorM(Error,Result) )

import Control.Applicative
import Control.Monad (when)

import Network.URI (parseURI)

import Data.Char (chr) 
import Data.Maybe (fromMaybe, isNothing)

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | NT parser state
data NTState = NTState
        { graphState :: RDFGraph            -- Graph under construction
        }

--  Return function to update graph in NT parser state,
--  using the supplied function of a graph
--  (use returned function with Parsec updateState)
updateGraph :: ( RDFGraph -> RDFGraph ) -> NTState -> NTState
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

type NTParser a = RDFParser NTState a

-- | Parse a string.
-- 
parseNT ::
  String -- ^ input in NTriples format.
  -> ParseResult
parseNT = either Error Result . parsefromString ntripleDoc

-- useful for testing
test :: String -> RDFGraph
test = either error id . parsefromString ntripleDoc

-- | Function to supply initial context and parse supplied term.
--
-- We augment the Parsec error with the context.
--
parsefromString :: 
    NTParser a      -- ^ parser to apply
    -> String       -- ^ input to be parsed
    -> Either String a
parsefromString parser input =
        let
            pstate = NTState
                    { graphState = emptyRDFGraph
                    }
            result = runParser parser pstate "" input
        in
            case result of
                Right res -> Right res
                Left  err -> Left $ annotateParsecError (lines input) err

-- helper routines

ignore :: NTParser a -> NTParser ()
ignore p = p *> return ()

fullStop :: NTParser ()
fullStop = ignore (char '.')

{-
lineFeed :: NTParser ()
lineFeed = ignore (char '\r')
-}

-- Add statement to graph in NT parser state

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> NTParser ()
addStatement s p o = updateState (updateGraph (addArc (arc s p o) ))

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
ntripleDoc = graphState <$> (sepBy line eoln *> optional eoln *> skipMany ws *> eof *> getState)

line :: NTParser ()
line = skipMany ws *> ignore (optional (comment <|> triple))

{-
ws	::=	space | tab	

Could use whiteSpace rule here, but that would permit
constructs (e.g. comments) where we do not support them.
-}
ws :: NTParser ()
ws = ignore (char ' ' <|> tab) <?> "white space (' ' or tab)"

{-
comment	::=	'#' ( character - ( cr | lf ) )*	
-}

comment :: NTParser ()
comment = char '#' *> skipMany (noneOf "\r\n") <?> "comment line"

{-
eoln	::=	cr | lf | cr lf	
-}

eoln :: NTParser ()
-- eoln = ignore (newline <|> (lineFeed *> optional newline))
eoln = ignore (try (string "\r\n") <|> string "\r" <|> string "\n")
       <?> "new line"
       
{-
name	::=	[A-Za-z][A-Za-z0-9]*	
-}

hChars, bChars :: String
hChars = ['a'..'z'] ++ ['A'..'Z']
bChars = hChars ++ ['0'..'9']

name :: NTParser String
name = (:) <$> oneOf hChars <*> many (oneOf bChars)

{-
triple	::=	subject ws+ predicate ws+ object ws* '.' ws*	

-}

triple :: NTParser ()
triple = do
  s <- subject
  skipMany1 ws
  p <- predicate
  skipMany1 ws
  o <- object
  skipMany ws
  fullStop
  skipMany ws
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
  ustr <- char '<' *> manyTill character (char '>')
  when (isNothing (parseURI ustr)) $
    fail ("Invalid URI: <" ++ ustr ++ ">")
  return $ makeUriScopedName ustr

urirefLbl :: NTParser RDFLabel
urirefLbl = Res <$> uriref

{-
nodeID	::=	'_:' name	
-}

nodeID :: NTParser RDFLabel
nodeID = Blank <$> (string "_:" *> name) <?> "blank node (_:label)"

{-  
literal	::=	langString | datatypeString	
langString	::=	'"' string '"' ( '@' language )?	
datatypeString	::=	'"' string '"' '^^' uriref	
language	::=	[a-z]+ ('-' [a-z0-9]+ )*
encoding a language tag.	
string	::=	character* with escapes as defined in section Strings	

-}

literal :: NTParser RDFLabel
literal = Lit <$> between (char '"') (char '"') (many character) <*> optionMaybe dtlang

dtlang :: NTParser ScopedName
dtlang = 
    (char '@' *> language)
    <|> (string "^^" *> uriref)

language :: NTParser ScopedName
language = do
  h <- many1 (oneOf ['a'..'z'])
  mt <- optionMaybe ( (:) <$> char '-' <*> many1 (oneOf (['a'..'z'] ++ ['0'..'9'])) )
  return $ langName $ h ++ fromMaybe "" mt

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

asciiChars :: String
asciiChars = map chr [0x20..0x7e]

asciiCharsNT :: String
asciiCharsNT = filter (`notElem` "\\\"") asciiChars

ntHexDigit :: NTParser Char
ntHexDigit = oneOf $ ['0'..'9'] ++ ['A'..'F']

hex4 :: NTParser Char
hex4 = do
  digs <- count 4 ntHexDigit
  let dstr = "0x" ++ digs
      dchar = read dstr :: Int
  return $ chr dchar
        
hex8 :: NTParser Char
hex8 = do
  digs <- count 8 ntHexDigit
  let dstr = "0x" ++ digs
      dchar = read dstr :: Int
  if dchar <= 0x10FFFF
    then return $ chr dchar
    else unexpected "\\UHHHHHHHH format is limited to a maximum of \\U0010FFFF"

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
character = (char '\\' *> protectedChar)
      <|> (oneOf asciiCharsNT <?> "ASCII character")

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
