--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ListHelpers
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines some generic list and related helper functions. Although
--  some routines are explicitly marked as deprecated, the intention is to
--  move this functionality into the modules that need it, or replace it by
--  other modules, where possible.
--
--------------------------------------------------------------------------------

{-
TODO:

The plan is to use modules such as Data.Set where appropriate.

-}

module Swish.Utils.ListHelpers
       ( -- list of Swish.RDF.xxx modules the routine is used in
         select  -- GraphMatch
       , deleteIndex -- Datatype
       , subset -- Proof, RDFProof, VarBinding [also defined in Utils.PartOrderedCollection]
       , equiv -- GraphMatch, RDFRuleset, SwishScript, VarBinding, Utils.LookupMap
       , addSetElem -- RDFGraph
       , headOrNothing -- VarBinding
       , pairUngroup -- GraphMatch
       , pairSort -- GraphMatch
       , pairGroup -- GraphMatch
       , breakAll -- SwishMain
       , powerSet -- ClassRestrictionRule, RDFProof
       , permutations -- VarBinding
       , listProduct -- RDFQuery
       , powerSequences_len -- RDFProof
       , flist -- Datatype, RDFProof, RDFRuleset, SwishScript, VarBinding
       , allp -- RDFQuery
       , anyp -- RDFQuery
        
        -- * Deprecated routines
        --
        -- | These routines will be removed at the next minor release of
        -- of Swish (@0.3.3@).
        --
       , mapset
       , pairsUngroup
       , ffold
       , hasPartitions
       , powerSequences
       , powerSequences_inf
       , allf
       , anyf
       , combinations -- used by powerSet
        
      )
where
  
import Data.Ord (comparing)  
import Data.List (union, intersect, sortBy, groupBy)

------------------------------------------------------------
--  Generic helpers
------------------------------------------------------------

-- |Select is like filter, except that it tests one list to select
--  elements from a second list.
select :: ( a -> Bool ) -> [a] -> [b] -> [b]
select _ [] []           = []
select f (e1:l1) (e2:l2)
    | f e1      = e2 : select f l1 l2
    | otherwise = select f l1 l2
select _ _ _    = error "select supplied with different length lists"

-- |Collect set of values from list under supplied mapping function
mapset :: Eq b => ( a -> b ) -> [a] -> [b]
mapset _ []    = []
mapset f (e:l) = [f e] `union` mapset f l

-- |Delete the n'th element of a list, returning the result
--
--  If the list doesn't have an n'th element, return the list unchanged.
--
deleteIndex :: [a] -> Int -> [a]
deleteIndex [] _ = []
deleteIndex xxs@(x:xs) n
    | n <  0    = xxs
    | n == 0    = xs
    | otherwise = x:deleteIndex xs (n-1)

{-
testdi1 = deleteIndex [1,2,3,4] 0    == [2,3,4]
testdi2 = deleteIndex [1,2,3,4] 1    == [1,3,4]
testdi3 = deleteIndex [1,2,3,4] 2    == [1,2,4]
testdi4 = deleteIndex [1,2,3,4] 3    == [1,2,3]
testdi5 = deleteIndex [1,2,3,4] 4    == [1,2,3,4]
testdi6 = deleteIndex [1,2,3,4] (-1) == [1,2,3,4]
testdi = and
    [ testdi1, testdi2, testdi3, testdi4, testdi5, testdi6 ]
-}

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

-- |Set partition test
--
--  Is it possible to be more efficient here?
--  Maybe something like sort\/merge\/compare?
hasPartitions   :: (Eq a) => [a] -> ([a],[a]) -> Bool
a `hasPartitions` (b1,b2) =
    null (b1 `intersect` b2) && (a `equiv` (b1 `union` b2))

-- |Add element to set

addSetElem :: (Eq a) => a -> [a] -> [a]
addSetElem e es = if e `elem` es then es else e:es

------------------------------------------------------------
--  Lists and Maybes
------------------------------------------------------------

-- |Return head of a list of @Maybe@'s, or @Nothing@ if list is empty
--
--  Use with @filter isJust@ to select a non-Nothing value from a
--  list when such a value is present.
--
headOrNothing :: [Maybe a] -> Maybe a
headOrNothing []    = Nothing
headOrNothing (a:_) = a

------------------------------------------------------------
--  Filter, ungroup, sort and group pairs by first member
------------------------------------------------------------

{-
pairSelect :: ((a,b) -> Bool) -> ((a,b) -> c) -> [(a,b)] -> [c]
pairSelect p f as = map f (filter p as)
-}

pairUngroup :: (a,[b]) -> [(a,b)]
pairUngroup (a,bs) = [ (a,b) | b <- bs ]

pairsUngroup :: [(a,[b])] -> [(a,b)]
pairsUngroup ps = [ (a,b) | (a,bs) <- ps, b <- bs ]

pairSort :: (Ord a) => [(a,b)] -> [(a,b)]
pairSort = sortBy (comparing fst)

pairGroup :: (Ord a) => [(a,b)] -> [(a,[b])]
pairGroup = map (factor . unzip) . groupBy eqFirst . pairSort 
    where
      -- as is not [] by construction, but would be nice to have
      -- this enforced by the types
      factor (as, bs) = (head as,bs)
      eqFirst a b     = fst a == fst b

------------------------------------------------------------
--  Separate list into sublists
------------------------------------------------------------

-- |Break list into a list of sublists, separated by element
--  satisfying supplied condition.
breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll _ [] = []
breakAll p s  = let (h,s') = break p s
                    in h : breakAll p (drop 1 s')

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
--  Permutations of a list
------------------------------------------------------------

--  This algorithm is copied from an email by S.D.Mechveliani
--  http://www.dcs.gla.ac.uk/mail-www/haskell/msg01936.html
permutations :: [a] -> [[a]]
permutations    []     = [[]]
permutations    (j:js) = addOne $ permutations js
    where
        addOne = foldr ((++) . ao) []

        ao []           = [[j]]
        ao (k:ks)       = (j:k:ks) : map (k:) (ao ks)

{-
testperm = permutations [1,2,3] ==
    [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}

------------------------------------------------------------
--  List product
------------------------------------------------------------

-- |Given a list of lists, construct a new list of lists where
--  each member of the new list is the same length as the original
--  list, and each member corresponds to a different choice of
--  one element from each of the original members in the
--  corresponding position.  Thus:
--
--  > listProduct [[a1,a2],[b1],[c1,c2]] =
--  >      [ [a1,b1,c1], [a1,b1,c2], [a2,b1,c1], [a2,b1,c2] ]
--
--  Note:  The length of the resulting list is the prodicty of
--  lengths of the components of the original list.  Thus, if
--  any member of the original list is empty then so is the
--  resulting list:
--
--  > listProduct [[a1,a2],[],[c1,c2]] = []
--
--  NOTE:  this is subsumed by 'sequence'
--
listProduct :: [[a]] -> [[a]]
listProduct []       = [[]]
listProduct (as:ass) = concat [ map (a:) (listProduct ass) | a <- as ]

{-
test1 = listProduct [["a1","a2"],["b1"],["c1","c2"]]
test2 = listProduct [["a1","a2"],[],["c1","c2"]]

lp []       = [[]]
lp (as:ass) = concatMap (\a -> (map (a:) (lp ass))) as
-}

------------------------------------------------------------
--  Powersequence (?) -- all sequences from some base values
------------------------------------------------------------

-- |Function to choose all sequences of any length
--  from a supplied set of values, returned in
--  increasing length.
powerSequences :: [a] -> [[a]]
powerSequences rs = concat $ powerSeq_bylen rs [[]]

-- |Construct list of lists of sequences of increasing length
powerSeq_bylen :: [a] -> [[a]] -> [[[a]]]
powerSeq_bylen rs ps = ps : powerSeq_bylen rs (powerSeq_next rs ps)

-- |Return sequences of length n+1 given original sequence
--  and list of all sequences of length n
powerSeq_next :: [a] -> [[a]] -> [[a]]
powerSeq_next rs rss = [ h:t | t <- rss, h <- rs ]

-- |Return all powersequences of a given length
powerSequences_len :: Int -> [a] -> [[a]]
powerSequences_len len rs = powerSeq_bylen rs [[]] !! len

-- |Return all powersequences of indefinite length
--  Observe that any such powersequence will consist of a sequence
--  of a finite length sequence followed by an indefinite number of
--  copies of the head of the base set.  To prevent duplicates, the
--  generator constructs only sequences that do not end in the first
--  member of the base set.
powerSequences_inf :: [a] -> [[a]]
powerSequences_inf rs =
    map (++pst) $ [] : concat (powerSeq_bylen rs psh)
    where
        psh = map (:[]) (tail rs)
        pst = repeat $ head rs

{- Powersequence tests
t0 = [1,2,3,4,5,6]
t1 = powerSequences t0
t2 = take 15 t1
t3 = powerSequences_len 3 t0
t4 = powerSequences_inf t0
t5 = map (take 6) $ take 15 t4
t6 = take 15 (powerSequences_len 6 t0)
t7 = t5 == t6
t8 = powerSequences_len1 3 t0
t9 = t8 == t3
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

-- |Fold result from list of functions applied to some value,
--  returning the result of the fold.
--
--  This is similar to the 'ap' function in the Monad library.
--
ffold :: (b->c->c) -> c -> [a->b] -> a -> c
ffold rf ri fs v = foldr rf ri (flist fs v)

{-
ffoldtest0 = ffold ge4and True [(1+),(2+),(3+)] 0     -- False
ffoldtest1 = ffold ge4and True [(1+),(2+),(3+)] 1     -- False
ffoldtest2 = ffold ge4and True [(1+),(2+),(3+)] 2     -- False
ffoldtest3 = ffold ge4and True [(1+),(2+),(3+)] 3     -- True
ge4and v b = (v>=4 && b)
ffoldtest  = and [not ffoldtest0,not ffoldtest1,not ffoldtest2,ffoldtest3]
-}

-- |Test if application of all functions in list to a given value
--  satisfies a given condition
--
allf :: (b->Bool)  -> [a->b] -> a -> Bool
allf pf fs a = all pf (flist fs a)

{-
allftest0 = allf (>=4) [(1+),(2+),(3+)] 0     -- False
allftest1 = allf (>=4) [(1+),(2+),(3+)] 1     -- False
allftest2 = allf (>=4) [(1+),(2+),(3+)] 2     -- False
allftest3 = allf (>=4) [(1+),(2+),(3+)] 3     -- True
allftest  = and [not allftest0,not allftest1,not allftest2,allftest3]
-}

-- |Test if application of any functions in list to a given value
--  satisfies a given condition
--
anyf :: (b->Bool)  -> [a->b] -> a -> Bool
anyf pf fs a = any pf (flist fs a)

{-
anyftest0 = anyf (>=4) [(1+),(2+),(3+)] 0     -- False
anyftest1 = anyf (>=4) [(1+),(2+),(3+)] 1     -- True
anyftest2 = anyf (>=4) [(1+),(2+),(3+)] 2     -- True
anyftest3 = anyf (>=4) [(1+),(2+),(3+)] 3     -- True
anyftest  = and [not anyftest0,anyftest1,anyftest2,anyftest3]
-}

-- |Test if a value satisfies all predicates in a list
--
allp :: [a->Bool] -> a -> Bool
allp ps a = and (flist ps a)

{-
allptest0 = allp [(>=1),(>=2),(>=3)] 0     -- False
allptest1 = allp [(>=1),(>=2),(>=3)] 1     -- False
allptest2 = allp [(>=1),(>=2),(>=3)] 2     -- False
allptest3 = allp [(>=1),(>=2),(>=3)] 3     -- True
allptest  = and [not allptest0,not allptest1,not allptest2,allptest3]
-}

-- |Test if a value satisfies any predicate in a list
--
anyp :: [a->Bool] -> a -> Bool
anyp ps a = or (flist ps a)

{-
anyptest0 = anyp [(>=1),(>=2),(>=3)] 0     -- False
anyptest1 = anyp [(>=1),(>=2),(>=3)] 1     -- True
anyptest2 = anyp [(>=1),(>=2),(>=3)] 2     -- True
anyptest3 = anyp [(>=1),(>=2),(>=3)] 3     -- True
anyptest  = and [not anyptest0,anyptest1,anyptest2,anyptest3]
-}


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
