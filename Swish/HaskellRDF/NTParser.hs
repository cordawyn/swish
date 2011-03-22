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
    , absoluteURI
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

import Network.URI (parseURI)

import Data.Maybe (fromMaybe)

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import Data.Char (chr) 


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
updateGraph :: ( RDFGraph -> RDFGraph ) -> ( NTState -> NTState )
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
-}
ntripleDoc :: NTParser RDFGraph
ntripleDoc = graphState <$> (many line *> eof *> getState)

{-
line	::=	ws* ( comment | triple )? eoln	
-}
line :: NTParser ()
line = skipMany ws *> optional (comment <|> triple) *> eoln

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
character	::=	[#x20-#x7E] /* US-ASCII space to decimal 126 */	

-}

asciiChars, asciiCharsNoGT, asciiCharsNoQuote :: String
asciiChars = map chr [0x20..0x7e]
asciiCharsNoGT = filter (/= '>') asciiChars
asciiCharsNoQuote = filter (/= '"') asciiChars

character, characterNoGT, characterNoQuote :: NTParser Char
character = oneOf asciiChars
characterNoGT = oneOf asciiCharsNoGT
characterNoQuote = oneOf asciiCharsNoQuote

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
-}

uriref :: NTParser ScopedName
uriref = makeUriScopedName <$> 
         between (char '<') (char '>') absoluteURI
         <?> "<absolute URI>"

urirefLbl :: NTParser RDFLabel
urirefLbl = Res <$> uriref

{-
absoluteURI	::=	character+ with escapes as defined in section URI References	

TODO: 
  - should we expand out the escapes?
  - should we return a URI rather than a string?
-}

absoluteURI :: NTParser String
absoluteURI = do
  ustr <- many1 characterNoGT
  case parseURI ustr of
    Just _ -> return ustr
    _ -> fail ("Invalid URI: <" ++ ustr ++ ">")

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

We follow the N3 Parser production rules here:
-}

literal :: NTParser RDFLabel
literal = Lit <$> between (char '"') (char '"') (many characterNoQuote) <*> optionMaybe dtlang

dtlang :: NTParser ScopedName
dtlang = 
    (char '@' *> language)
    <|> (string "^^" *> uriref)

language :: NTParser ScopedName
language = do
  h <- many1 (oneOf ['a'..'z'])
  mt <- optionMaybe ( (:) <$> char '-' <*> many1 (oneOf (['a'..'z'] ++ ['0'..'9'])) )
  return $ langName $ h ++ fromMaybe "" mt


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
