--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  MiscHelpers
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines some generic list and related helper functions
--  used by the graph handling code.
--
--------------------------------------------------------------------------------

module Swish.Utils.MiscHelpers
      ( assert, lower, stricmp, quote, hash, hashModulus )
where

import Data.Char
    ( toLower )

------------------------------------------------------------
--  assert test
------------------------------------------------------------

assert :: Bool -> String -> a -> a
assert cond msg expr = if not cond then error msg else expr

------------------------------------------------------------
--  Generate lowercase form of supplied string
------------------------------------------------------------

lower :: String -> String
lower = foldr ((:) . toLower) "" 

------------------------------------------------------------
--  Case insensitive compare.
------------------------------------------------------------
--
--  Should be used only for values using just the US ASCII
--  character set.  Use with richer character sets can yield
--  surprising results.

stricmp :: String -> String -> Bool
stricmp (c1:s1) (c2:s2) = toLower c1 == toLower c2 && stricmp s1 s2
stricmp []      []      = True
stricmp _       _       = False

------------------------------------------------------------
--  Generate quoted form of supplied string:
------------------------------------------------------------
--
--  [[[TODO: The list of quoting options here is incomplete]]]

quote :: String -> String
quote  st = ['"'] ++ quote1 st ++ ['"']

quote1 :: String -> String
quote1 ('"': st)    = '\\':'"' : quote1 st
quote1 ('\\':st)    = '\\':'\\': quote1 st
quote1 ('\n':st)    = '\\':'n': quote1 st
quote1 ('\r':st)    = '\\':'r': quote1 st
quote1 (c:st)       = c: quote1 st
quote1 []           = ""

------------------------------------------------------------
--  Hash function and values
------------------------------------------------------------
--
--  Simple hash function based on Sedgewick, Algorithms in C, p 233
--  (choose mx*cm+255 < maxBound)
--  'seed' is an additional parameter that allows the function
--  to be varied for re-hashing.

hashModulus :: Int
hashModulus = 16000001

hash :: Int -> String -> Int
hash seed = hash1 seed (64+seed) hashModulus 

hash1 :: Int -> Int -> Int -> String -> Int
hash1 sofar cm mx (c:str) = hash1 (( sofar*cm + fromEnum c ) `rem` mx) cm mx str
hash1 sofar _ _ []        = sofar


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
