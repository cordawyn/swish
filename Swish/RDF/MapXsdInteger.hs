--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  MapXsdInteger
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines the datatytpe mapping and relation values
--  used for RDF dataype xsd:integer
--
--------------------------------------------------------------------------------

module Swish.RDF.MapXsdInteger
    ( mapXsdInteger
    )
where

import Swish.RDF.Datatype
    ( DatatypeMap(..)
    )

import Text.ParserCombinators.Parsec

------------------------------------------------------------
--  Implementation of DatatypeMap for xsd:integer
------------------------------------------------------------

-- |mapXsdInteger contains functions that perform lexical-to-value
--  and value-to-canonical-lexical mappings for xsd:integer values
--
mapXsdInteger :: DatatypeMap Integer
mapXsdInteger = DatatypeMap
    { -- mapL2V :: String -> Maybe Integer
      mapL2V = fromString
      
      -- mapV2L :: Integer -> Maybe String
    , mapV2L = Just . show
    }

-- previously there was a regular expression used to check
-- the string before parsing; this has been replaced by a
-- simple parsec parser.
-- 
fromString :: String -> Maybe Integer
fromString input =
  case runParser intVal "" "" input of
    Right ival -> Just ival
    Left _     -> Nothing
  
leadingSign :: CharParser st Integer
leadingSign = 
  (char '-' >> return (-1)) <|> 
  (char '+' >> return 1) <|>
  return 1

intVal :: CharParser st Integer
intVal = do
  sign <- leadingSign
  aval <- many1 (oneOf "0123456789")
  notFollowedBy anyChar
  return $ sign * read aval
    
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
