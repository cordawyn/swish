{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFParser
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  Support for the RDF Parsing modules.
--
--------------------------------------------------------------------------------

module Swish.RDF.RDFParser
    ( SpecialMap
    , mapPrefix
              
    -- tables
    , prefixTable, specialTable

    -- parser
    , ParseResult
    , ignore
    , char
    , ichar
    , string
    , symbol
    , lexeme
    , notFollowedBy
    , whiteSpace
    , skipMany
    , skipMany1
    , endBy
    , sepEndBy
    , sepEndBy1
    , manyTill
    , noneOf
    , eoln
    , fullStop
    , mkTypedLit
    , hex4
    , hex8
    )
where

import Swish.RDF.RDFGraph
    ( RDFGraph, RDFLabel(..)
    , NamespaceMap
    )

import Swish.Utils.LookupMap
    ( LookupMap(..)
    , mapFind 
    )

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    )

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceOWL
    , namespaceLOG
    , rdf_type
    , rdf_first, rdf_rest, rdf_nil
    , owl_sameAs, log_implies
    , default_base
    )

import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read as R

import Text.ParserCombinators.Poly.StateText

import Data.Char (isSpace, isHexDigit, chr)
import Data.Maybe (fromMaybe)

-- Code

-- | Type for special name lookup table
type SpecialMap = LookupMap (String,ScopedName)

-- | Lookup prefix in table and return URI or 'prefix:'
mapPrefix :: NamespaceMap -> String -> String
mapPrefix ps pre = mapFind (pre++":") pre ps

-- | Define default table of namespaces
prefixTable :: [Namespace]
prefixTable =   [ namespaceRDF
                , namespaceRDFS
                , namespaceRDFD     -- datatypes
                , namespaceOWL
                , namespaceLOG
                , Namespace "" "#" -- is this correct?
                ]

{-|
Define default special-URI table.
The optional argument defines the initial base URI.
-}
specialTable :: Maybe ScopedName -> [(String,ScopedName)]
specialTable mbase =
  [ ("a",         rdf_type    ),
    ("equals",    owl_sameAs  ),
    ("implies",   log_implies ),
    ("listfirst", rdf_first   ),
    ("listrest",  rdf_rest    ),
    ("listnull",  rdf_nil     ),
    ("base",      fromMaybe default_base mbase ) 
  ]

-- Parser routines, heavily based on Parsec

type ParseResult = Either String RDFGraph

ignore :: (Applicative f) => f a -> f ()
ignore f = f *> pure ()

char :: Char -> Parser s Char
char c = satisfy (==c)

ichar :: Char -> Parser s ()
ichar = ignore . char

-- TODO: is there a better way to do this?
string :: String -> Parser s String
string s = mapM char s
  
skipMany :: Parser s a -> Parser s ()
skipMany = ignore . many
  
skipMany1 :: Parser s a -> Parser s ()
skipMany1 = ignore . many1
  
endBy :: Parser s a -> Parser s b -> Parser s [a]
endBy p sep = many (p <* sep)

sepEndBy :: Parser s a -> Parser s b -> Parser s [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

-- is the separator optional?
sepEndBy1 :: Parser s a -> Parser s b -> Parser s [a]
sepEndBy1 p sep = do
  x <- p
  (sep *> ((x:) <$> sepEndBy p sep)) <|> return [x]
  
manyTill :: Parser s a -> Parser s b -> Parser s [a]
manyTill p end = go
  where
    go = (end *> return [])
         <|>
         ((:) <$> p <*> go)


noneOf :: String -> Parser s Char           
noneOf istr = satisfy (`notElem` istr)
           
fullStop :: Parser s ()
fullStop = ichar '.'

eoln :: Parser s ()
-- eoln = ignore (newline <|> (lineFeed *> optional newline))
-- eoln = ignore (try (string "\r\n") <|> string "\r" <|> string "\n")
eoln = ignore (oneOf [string "\r\n", string "\r", string "\n"])
       
notFollowedBy :: (Char -> Bool) -> Parser s ()
notFollowedBy p = do
  c <- next
  if p c 
    then fail $ "Unexpected character: " ++ show [c]
    else reparse $ L.singleton c

symbol :: String -> Parser s String
symbol = lexeme . string

lexeme :: Parser s a -> Parser s a
lexeme p = p <* whiteSpace

whiteSpace :: Parser s ()
whiteSpace = skipMany (simpleSpace <|> oneLineComment)

simpleSpace :: Parser s ()
simpleSpace = ignore $ many1Satisfy isSpace

oneLineComment :: Parser s ()
oneLineComment = ichar '#' *> manySatisfy (/= '\n') *> pure ()

{-

Not sure we can get this with polyparse

-- | Annotate a Parsec error with the local context - i.e. the actual text
-- that caused the error and preceeding/succeeding lines (if available)
--
annotateParsecError :: 
    Int -- ^ the number of extra lines to include in the context (<=0 is ignored)
    -> [String] -- ^ text being parsed
    -> ParseError -- ^ the parse error
    -> String -- ^ Parsec error with additional context
annotateParsecError extraLines ls err = 
    -- the following is based on the show instance of ParseError
    let ePos = errorPos err
        lNum = sourceLine ePos
        cNum = sourceColumn ePos
        -- it is possible to be at the end of the input so need
        -- to check; should produce better output than this in this
        -- case
        nLines = length ls
        ln1 = lNum - 1
        eln = max 0 extraLines
        lNums = [max 0 (ln1 - eln) .. min (nLines-1) (ln1 + eln)]
        
        beforeLines = map (ls !!) $ filter (< ln1) lNums
        afterLines  = map (ls !!) $ filter (> ln1) lNums
        
        -- in testing was able to get a line number after the text so catch this
        -- case; is it still necessary?
        errorLine = if ln1 >= nLines then "" else ls !! ln1
        arrowLine = replicate (cNum-1) ' ' ++ "^"
        finalLine = "(line " ++ show lNum ++ ", column " ++ show cNum ++ " indicated by the '^' sign above):"
        
        eHdr = "" : beforeLines ++ errorLine : arrowLine : afterLines ++ [finalLine]
        eMsg = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
               (errorMessages err)

    in unlines eHdr ++ eMsg

-}

-- | Create a typed literal.
mkTypedLit ::
  ScopedName -- ^ the type
  -> String -- ^ the value
  -> RDFLabel
mkTypedLit u v = Lit v (Just u)

{-
Handle hex encoding; the spec for N3 and NTriples suggest that
only upper-case A..F are valid but you can find lower-case values
out there so support these too.
-}

hexDigit :: Parser a Char
-- hexDigit = satisfy (`elem` ['0'..'9'] ++ ['A'..'F'])
hexDigit = satisfy isHexDigit

hex4 :: Parser a Char
hex4 = do
  digs <- exactly 4 hexDigit
  let mhex = R.hexadecimal (T.pack digs)
  case mhex of
    Left emsg     -> failBad $ "Internal error: unable to parse hex4: " ++ emsg
    Right (v, "") -> return $ chr v
    Right (_, vs) -> failBad $ "Internal error: hex4 has remained of " ++ T.unpack vs
        
hex8 :: Parser a Char
hex8 = do
  digs <- exactly 8 hexDigit
  let mhex = R.hexadecimal (T.pack digs)
  case mhex of
    Left emsg     -> failBad $ "Internal error: unable to parse hex8: " ++ emsg
    Right (v, "") -> if v <= 0x10FFFF
                     then return $ chr v
                     else failBad "\\UHHHHHHHH format is limited to a maximum of \\U0010FFFF"
    Right (_, vs) -> failBad $ "Internal error: hex8 has remained of " ++ T.unpack vs
        
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
