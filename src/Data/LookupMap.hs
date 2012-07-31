{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  LookupMap
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  A lot of LANGUAGE extensions...
--
--  This module defines a lookup table format and associated functions
--  used by the graph matching code.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
--  Generic list-of-pairs lookup functions
------------------------------------------------------------

module Data.LookupMap
    ( LookupEntryClass(..), LookupMap(..)
    , emptyLookupMap, makeLookupMap, listLookupMap
    , reverseLookupMap
    , keyOrder
    , mapFind, mapFindMaybe, mapContains
    , mapReplace, mapReplaceOrAdd, mapReplaceAll, mapReplaceMap
    , mapAdd, mapAddIfNew
    , mapDelete, mapDeleteAll
    , mapApplyToAll, mapTranslate
    , mapEq, mapKeys, mapVals
    , mapSelect, mapMerge
    , mapTranslateKeys, mapTranslateVals
    , mapTranslateEntries, mapTranslateEntriesM

    )
    where

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as L

import Control.Arrow (first, second)

import Data.Ord (comparing)

import Swish.Utils.ListHelpers (equiv)

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 701)
import Data.Tuple (swap)
#else
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
#endif

------------------------------------------------------------
--  Class for lookup map entries
------------------------------------------------------------

-- |@LookupEntryClass@ defines essential functions of any datatype
--  that can be used to make a 'LookupMap'.
--
--  Minimal definition: @newEntry@ and @keyVal@
--
class (Eq k, Show k) => LookupEntryClass a k v | a -> k, a -> v
    where
        newEntry    :: (k,v) -> a
        keyVal      :: a -> (k,v)
        
        entryKey    :: a -> k
        entryKey = fst . keyVal
        
        entryVal    :: a -> v
        entryVal = snd . keyVal
                             
        entryEq     :: (Eq v) => a -> a -> Bool
        entryEq e1 e2 = keyVal e1 == keyVal e2
        
        entryShow   :: (Show v) => a -> String
        entryShow e = show k ++ ":" ++ show v where (k,v) = keyVal e
                                                    
        kmap :: (LookupEntryClass a2 k2 v) => (k -> k2) -> a -> a2
        kmap f = newEntry . first f . keyVal
        
        vmap :: (LookupEntryClass a2 k v2) => (v -> v2) -> a -> a2
        vmap f = newEntry . second f . keyVal

-- |Predefine a pair of appropriate values as a valid lookup table entry
--  (i.e. an instance of LookupEntryClass).
--
instance (Eq k, Show k) => LookupEntryClass (k,v) k v where
    newEntry = id
    keyVal   = id

--  Note:  the class constraint that a is an instance of 'LookupEntryClass'
--  is not defined here, for good reasons (which I forget right now, but
--  something to do with the method dictionary being superfluous on
--  an algebraic data type).
--
-- |Define a lookup map based on a list of values.
--
data LookupMap a = LookupMap [a]
  deriving (Functor, F.Foldable, T.Traversable)

{- 
TODO: could add

instance Monoid (LookupMap a) where
    mempty = LookupMap []
    mappend = mapMerge

but may need constraints on a, do not
want to add instances at this time, and is
it really useful? 

-}

gLM :: LookupMap a -> [a]
gLM (LookupMap es) = es

-- TODO:  See also 'mapEq'
--  (why not just use that for the Eq instance?  I don't know:  it's probably historic.)
--

-- |Define equality of 'LookupMap' values based on equality of entries.
--
--  (This is possibly a poor definition, as it is dependent on ordering
--  of list members.  But it passes all current test cases, and is used
--  only for testing.)
--
instance (Eq a) => Eq (LookupMap a) where
    LookupMap es1 == LookupMap es2 = es1 == es2

-- |Define Show instance for LookupMap based on Showing the
-- list of entries.
--
instance (Show a ) => Show (LookupMap a) where
    show (LookupMap es) = "LookupMap " ++ show es

{-
TODO: should the LookupEntryClass constraint be removed from
emptyLookupMap and makeLookupMap?

I guess not since LookupMap is exported, so users can use
that if they do not need the constraint.
-}

-- |Empty lookup map of arbitrary (i.e. polymorphic) type.
--
emptyLookupMap :: (LookupEntryClass a k v) => LookupMap a
emptyLookupMap = LookupMap []

-- |Function to create a `LookupMap` from a list of entries.
--
--  Currently, this is trivial but future versions could be
--  more substantial.
--
makeLookupMap :: (LookupEntryClass a k v) => [a] -> LookupMap a
makeLookupMap = LookupMap

-- |Return list of lookup map entries.
--
--  Currently, this is trivial but future versions could be
--  more substantial.
--
listLookupMap :: (LookupEntryClass a k v) => LookupMap a -> [a]
listLookupMap = gLM

-- |Given a lookup map entry, return a new entry that can be used
--  in the reverse direction of lookup.  This is used to construct
--  a reverse LookupMap.
--
reverseEntry :: (LookupEntryClass a1 k v, LookupEntryClass a2 v k)
    => a1 -> a2
reverseEntry = newEntry . swap . keyVal

-- |Given a lookup map, return a new map that can be used
--  in the opposite direction of lookup.
--
reverseLookupMap :: (LookupEntryClass a1 b c, LookupEntryClass a2 c b)
    => LookupMap a1 -> LookupMap a2
reverseLookupMap = fmap reverseEntry

-- |Given a pair of lookup entry values, return the ordering of their
--  key values.
--
keyOrder :: (LookupEntryClass a k v, Ord k)
    =>  a -> a -> Ordering
keyOrder = comparing entryKey

--  Local helper function to build a new LookupMap from
--  a new entry and an exiting map.
--
mapCons :: a -> LookupMap a -> LookupMap a
mapCons e (LookupMap es) = LookupMap (e:es)

-- |Find key in lookup map and return corresponding value,
--  otherwise return default supplied.
--
mapFind :: (LookupEntryClass a k v) => v -> k -> LookupMap a -> v
mapFind def key (LookupMap es) = foldr match def es where
    match ent alt
        | key == entryKey ent   = entryVal ent
        | otherwise             = alt

-- |Find key in lookup map and return Just the corresponding value,
--  otherwise return Nothing.
--
mapFindMaybe :: (LookupEntryClass a k v) => k -> LookupMap a -> Maybe v
mapFindMaybe key (LookupMap es) = foldr match Nothing es where
    match ent alt
        | key ==  entryKey ent  = Just (entryVal ent)
        | otherwise             = alt

-- |Test to see if key is present in the supplied map
--
mapContains :: (LookupEntryClass a k v) =>
    LookupMap a -> k -> Bool
mapContains (LookupMap es) key  = any match es where
    match ent = key == entryKey ent

-- |Replace an existing occurrence of a key a with a new key-value pair.
--    
--  The resulting lookup map has the same form as the original in all
--  other respects.  Assumes exactly one occurrence of the supplied key.
--
mapReplace :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapReplace (LookupMap (e:es)) newe
    | entryKey e == entryKey newe       = LookupMap (newe:es)
    | otherwise                         = mapAdd more e where
        more = mapReplace (LookupMap es) newe
mapReplace _ newe =
    error ("mapReplace: Key value not found in lookup table: "++
           Prelude.show (entryKey newe))

-- |Replace an existing occurrence of a key a with a new key-value pair,
--  or add a new key-value pair if the supplied key is not already present.
--
mapReplaceOrAdd :: (LookupEntryClass a k v) =>
    a -> LookupMap a -> LookupMap a
mapReplaceOrAdd newe (LookupMap (e:es))
    | entryKey e == entryKey newe       = LookupMap (newe:es)
    | otherwise                         = mapCons e more where
        more = mapReplaceOrAdd newe (LookupMap es)
mapReplaceOrAdd newe (LookupMap [])     = LookupMap [newe]

-- |Replace any occurrence of a key a with a new key-value pair.
--
--  The resulting lookup map has the same form as the original in all
--  other respects.
--
mapReplaceAll :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapReplaceAll (LookupMap (e:es)) newe   = mapCons e' more where
    more = mapReplaceAll (LookupMap es) newe
    e'   = if entryKey e == entryKey newe then newe else e
mapReplaceAll (LookupMap []) _          = LookupMap []

-- |Replace any occurrence of a key in the first argument with a
--  corresponding key-value pair from the second argument, if present.
--
--  This could be implemented by multiple applications of 'mapReplaceAll',
--  but is arranged differently so that only one new @LookupMap@ value is
--  created.
--
--  Note:  keys in the new map that are not present in the old map
--  are not included in the result map
--
mapReplaceMap :: (LookupEntryClass a k v) =>
    LookupMap a -> LookupMap a -> LookupMap a
mapReplaceMap (LookupMap (e:es)) newmap = mapCons e' more where
    more  = mapReplaceMap (LookupMap es) newmap
    e'    = newEntry (k, mapFind v k newmap)
    (k,v) = keyVal e
mapReplaceMap (LookupMap []) _ = LookupMap []

-- |Add supplied key-value pair to the lookup map.
--
--  This is effectively an optimized case of 'mapReplaceOrAdd' or 'mapAddIfNew',
--  where the caller guarantees to avoid duplicate key values.
--
mapAdd :: LookupMap a -> a -> LookupMap a
mapAdd emap e = mapCons e emap

-- |Add supplied key-value pair to the lookup map,
--  only if the key value is not already present.
--
mapAddIfNew :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapAddIfNew emap e = if mapContains emap (entryKey e)
                        then emap
                        else mapCons e emap

-- |Delete supplied key value from the lookup map.
--
--  This function assumes exactly one occurrence.
--
mapDelete :: (LookupEntryClass a k v) =>
    LookupMap a -> k -> LookupMap a
mapDelete (LookupMap (e:es)) k
    | k == entryKey e   = LookupMap es
    | otherwise         = mapCons e more where
        more = mapDelete (LookupMap es) k
mapDelete _ k =
    error ("mapDelete: Key value not found in lookup table: " ++ Prelude.show k)

-- |Delete any occurrence of a supplied key value from the lookup map.
--
mapDeleteAll :: (LookupEntryClass a k v) =>
    LookupMap a -> k -> LookupMap a
mapDeleteAll (LookupMap (e:es)) k =
    if entryKey e == k then more else mapCons e more where
        more = mapDeleteAll (LookupMap es) k
mapDeleteAll (LookupMap []) _ = LookupMap []

-- |Return a list of values obtained by applying a function to each key
--  in the map.  Creates an alternative set of values that can be
--  retrieved using mapTranslate.
--
mapApplyToAll :: (LookupEntryClass a k v) =>
    LookupMap a -> (k -> w) -> [w]
mapApplyToAll es f = gLM $ fmap (f . entryKey) es

-- |Find a node in a lookup map list, and returns the
--  corresponding value from a supplied list.  The appropriate ordering
--  of the list is not specified here, but an appropriately ordered list
--  may be obtained by 'mapApplyToAll'.
--
mapTranslate :: (LookupEntryClass a k v) =>
    LookupMap a -> [w] -> k -> w -> w
mapTranslate (LookupMap (e:es)) (w:ws) k def
    | k == entryKey e   = w
    | otherwise         = mapTranslate (LookupMap es) ws k def
mapTranslate _ _ _ def = def

-- |Compare two lookup maps for equality.
--
--  Two maps are equal if they have the same set of keys, and if
--  each key maps to an equivalent value.
--
mapEq :: (LookupEntryClass a k v, Eq v) =>
    LookupMap a -> LookupMap a -> Bool
mapEq es1 es2 =
    ks1 `equiv` ks2 &&
    and [ mapFindMaybe k es1 == mapFindMaybe k es2 | k <- ks1 ]
    where
        ks1 = mapKeys es1
        ks2 = mapKeys es2

-- |Return the list of keys in a supplied LookupMap
--
mapKeys :: (LookupEntryClass a k v) =>
    LookupMap a -> [k]
mapKeys (LookupMap es) = L.nub $ map entryKey es

-- |Return list of distinct values in a supplied LookupMap
--
mapVals :: (Eq v, LookupEntryClass a k v) =>
    LookupMap a -> [v]
mapVals (LookupMap es) = L.nub $ map entryVal es

-- |Select portion of a lookup map that corresponds to
--  a supplied list of keys
--
mapSelect :: (LookupEntryClass a k v) =>
    LookupMap a -> [k] -> LookupMap a
mapSelect (LookupMap es) ks =
    LookupMap $ filter (keyIn ks) es
    where
        keyIn iks e = entryKey e `elem` iks

-- |Merge two lookup maps, ensuring that if the same key appears
--  in both maps it is associated with the same value.
--
mapMerge :: (LookupEntryClass a k v, Eq a, Show a, Ord k) =>
    LookupMap a -> LookupMap a -> LookupMap a
mapMerge (LookupMap s1) (LookupMap s2) =
    LookupMap $ merge (L.sortBy keyOrder s1) (L.sortBy keyOrder s2)
    where
        merge es1 [] = es1
        merge [] es2 = es2
        merge es1@(e1:et1) es2@(e2:et2) =
            case keyOrder e1 e2 of
                LT -> e1 : merge et1 es2
                GT -> e2 : merge es1 et2
                EQ -> if e1 /= e2
                        then error ("mapMerge key conflict: " ++ show e1
                                    ++ " with " ++ show e2)
                        else e1 : merge et1 et2

-- |An fmap-like function that returns a new lookup map that is a
--  copy of the supplied map with entry keys replaced according to
--  a supplied function.
--
mapTranslateKeys :: (LookupEntryClass a1 k1 v, LookupEntryClass a2 k2 v) =>
    (k1 -> k2) -> LookupMap a1 -> LookupMap a2
mapTranslateKeys f = fmap (kmap f)

-- |An fmap-like function that returns a new lookup map that is a
--  copy of the supplied map with entry values replaced according to
--  a supplied function.
--
mapTranslateVals :: (LookupEntryClass a1 k v1, LookupEntryClass a2 k v2) =>
    (v1 -> v2) -> LookupMap a1 -> LookupMap a2
mapTranslateVals f = fmap (vmap f)

-- |A function that returns a new lookup map that is a copy of the
--  supplied map with complete entries replaced according to
--  a supplied function.
--
mapTranslateEntries :: (a1 -> a2) -> LookupMap a1 -> LookupMap a2
mapTranslateEntries = fmap

-- |A monadic form of `mapTranslateEntries` which is
-- the same as `Data.Traversable.mapM`.
--
-- Since `LookupMap` now has a `Data.Traversable.Traversable` instance
-- this is just `T.mapM`.
--
mapTranslateEntriesM :: (Monad m)
    => (a1 -> m a2) -> LookupMap a1 -> m (LookupMap a2)
mapTranslateEntriesM = T.mapM 
{-
mapTranslateEntriesM f (LookupMap es) =
    do  { m2 <- mapM f es
        ; return $ LookupMap m2
        }
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
