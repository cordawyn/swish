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

-- basic little parser for integer values;
-- do we need to bother about rejecting 
-- input like "-000" or "+0"?
--
fromString :: String -> Maybe Integer
fromString ('-':xs) = fs False xs
fromString ('+':xs) = fs True xs
fromString xs       = fs True xs

fs :: Bool -> String -> Maybe Integer
fs _ [] = Nothing
fs f is = 
  let val = go is []
      
      go [] ys = Just $ read $ reverse ys
      go (x:xs) ys | x `elem` ['0'..'9'] = go xs (x:ys)
                   | otherwise           = Nothing
        
   in case f of
    True -> val
    False -> fmap ((-1) *) val

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
