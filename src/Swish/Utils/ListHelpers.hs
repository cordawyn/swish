--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ListHelpers
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines some generic list and related helper functions. The plan
--  is to move to using functionality from other modules (e.g. containers) where
--  possible.
--
--------------------------------------------------------------------------------

module Swish.Utils.ListHelpers
       ( -- list of Swish.xxx modules the routine is used in
         subset -- Proof, RDF.Proof, VarBinding [also defined in Utils.PartOrderedCollection]
       , equiv -- GraphMatch, RDF.Ruleset, SwishScript, VarBinding, Utils.LookupMap
       , powerSet -- ClassRestrictionRule, RDF.Proof
       , flist -- Datatype, RDF.Proof, RDF.Ruleset, SwishScript, VarBinding
        
      )
where

------------------------------------------------------------
--  Set functions
--
--  NOTE: to change to Data.Set then Eq a constraint will 
--        likely need changing to Ord a
------------------------------------------------------------

-- |Subset test

subset          :: (Eq a) => [a] -> [a] -> Bool
a `subset` b    = and [ ma `elem` b | ma <- a ]

-- |Set equivalence test

equiv           :: (Eq a) => [a] -> [a] -> Bool
a `equiv` b     = a `subset` b && b `subset` a

------------------------------------------------------------
--  Powerset
------------------------------------------------------------

--  [[[TBD... there's a much better implementation in my email,
--     from Christopher Hendrie.  This is the raw code.]]]
{-
>ranked_powerset :: [a] -> [[[a]]]
>ranked_powerset = takeWhile (not . null) . foldr next_powerset ([[]] :
repeat [])
>
>next_powerset :: a -> [[[a]]] -> [[[a]]]
>next_powerset x r = zipWith (++) ([] : map (map (x:)) r) r
>
>powerset :: [a] -> [[a]]
>powerset = tail . concat . ranked_powerset
-}

-- |Powerset of a list, in ascending order of size.
--  Assumes the supplied list has no duplicate elements.
powerSet :: [a] -> [[a]]
powerSet as =
    concatMap (`combinations` as) [1..length as]

-- |Combinations of n elements from a list, each being returned in the
--  order that they appear in the list.
combinations :: Int -> [a] -> [[a]]
combinations _ []       = []        -- Don't include empty combinations
combinations n as@(ah:at)
    | n <= 0            = [[]]
    | n >  length as    = []
    | n == length as    = [as]
    | otherwise         = map (ah:) (combinations (n-1) at) ++
                          combinations n at

{-
-- |Return list of integers from lo to hi.
intRange :: Int -> Int -> [Int]
intRange lo hi = take (hi-lo+1) (iterate (+1) 1)
-}

{-
-- Tests
testcomb0 = combinations 0 "abcd" -- []
testcomb1 = combinations 1 "abcd" -- ["a","b","c","d"]
testcomb2 = combinations 2 "abcd" -- ["ab","ac","ad","bc","bd","cd"]
testcomb3 = combinations 3 "abcd" -- ["abc","abd","acd","bcd"]
testcomb4 = combinations 4 "abcd" -- ["abcd"]
testcomb5 = combinations 5 "abcd" -- []
testpower = powerSet "abc"        -- ["a","b","c","ab","ac","bc","abc"]
-}

------------------------------------------------------------
--  Functions, lists and monads
------------------------------------------------------------

-- |Apply list of functions to some value, returning list of results.
--  It's kind of like an converse map.
--
--  This is similar to the 'ap' function in the Monad library.
--
flist :: [a->b] -> a -> [b]
flist fs a = map ($ a) fs

{-
flisttest = flist [(1*),(2*),(3*)] 5 -- [5,10,15]
-}

{-

-- |A more generalized form of flist that works with arbitrary Monads.
--  (Suggested by Derek Elkin.)

fmonad :: Monad m => m (a->b) -> a -> m b
fmonad fm a =
    do  { f <- fm
        ; return $ f a
        }

-}

{-
fmonadtest = fmonad [(1*),(2*),(3*)] 3 -- [3,6,9]
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
