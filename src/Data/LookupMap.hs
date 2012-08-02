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
    , mapReplace, mapReplaceAll, mapReplaceMap
    , mapAdd, mapAddIfNew
    , mapDelete, mapDeleteAll
    , mapEq, mapKeys, mapVals
    , mapMerge
    )
    where

import Control.Arrow (first, second)

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)

import Swish.Utils.ListHelpers (equiv)

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as L

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

-- |Define a lookup map based on a list of values.
--
data LookupMap a = LookupMap [a]
  deriving (Functor, F.Foldable, T.Traversable)

{-

To allow this Monoid instance, we would need UndecidableInstances.
Also, mapMerge can error out which is not what we would want.

instance (LookupEntryClass a k v, Eq a, Show a, Ord k) => Monoid (LookupMap a) where
    mempty = LookupMap []
    mappend = mapMerge

We could use the following (perhaps with a L.nub on the result before sticling back
into LookupMap) but it is unclear what the semantics are for repeated keys; it is
likely to be left-biased but would leave duplicate keys in the list which could
cause confusion at a later time (e.g. key removal). Many of the routines assume a single
key (or single key,value) pair.

instance (Eq a) => Monoid (LookupMap a) where
    mempty = LookupMap []
    (LookupMap a) `mappend` (LookupMap b) =
        LookupMap (a `mappend` b))
-}

gLM :: LookupMap a -> [a]
gLM (LookupMap es) = es

-- |Define equality of 'LookupMap' values based on equality of entries.
--
--  (This is possibly a poor definition, as it is dependent on ordering
--  of list members.  But it passes all current test cases, and is used
--  only for testing.)
--
instance (Eq a) => Eq (LookupMap a) where
    (==) = (==) `on` gLM

-- |Define Show instance for LookupMap based on Showing the
-- list of entries.
--
instance (Show a ) => Show (LookupMap a) where
    show (LookupMap es) = "LookupMap " ++ show es

-- |Empty lookup map of arbitrary (i.e. polymorphic) type.
--
emptyLookupMap :: (LookupEntryClass a k v) => LookupMap a
emptyLookupMap = LookupMap []

-- |Function to create a `LookupMap` from a list of entries.
--
makeLookupMap :: 
    (LookupEntryClass a k v) 
    => [a]  -- ^ This list is not checked for duplicate entries, or
            -- entries with the same key but different values.
    -> LookupMap a
makeLookupMap = LookupMap

-- |Returns a list of lookup map entries.
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

-- |Find key in lookup map and return corresponding value,
--  otherwise return default supplied.
--
mapFind :: 
    (LookupEntryClass a k v) 
    => v    -- ^ The default value.
    -> k 
    -> LookupMap a -> v
mapFind def key = fromMaybe def . mapFindMaybe key

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

-- |Replace the first occurrence of a key a with a new key-value pair,
--  or add a new key-value pair if the supplied key is not already present.
--
mapReplace :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapReplace (LookupMap []) newe      = LookupMap [newe]
mapReplace (LookupMap (e:es)) newe
    | entryKey e == entryKey newe   = LookupMap (newe:es)
    | otherwise                     = mapAdd more e where
        more = mapReplace (LookupMap es) newe

-- |Replace all occurrence of a key a with a new key-value pair.
--
--  The resulting lookup map has the same form as the original in all
--  other respects.
--
mapReplaceAll :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapReplaceAll l@(LookupMap []) _        = l
mapReplaceAll (LookupMap (e:es)) newe   = mapAdd more e' where
    more = mapReplaceAll (LookupMap es) newe
    e'   = if entryKey e == entryKey newe then newe else e

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
mapReplaceMap l@(LookupMap []) _ = l
mapReplaceMap (LookupMap (e:es)) newmap = mapAdd more e' where
    more  = mapReplaceMap (LookupMap es) newmap
    e'    = newEntry (k, mapFind v k newmap)
    (k,v) = keyVal e

-- |Add supplied key-value pair to the lookup map.
--
--  This is effectively an optimized case of 'mapReplace' or 'mapAddIfNew',
--  where the caller guarantees to avoid duplicate key values.
--
mapAdd :: LookupMap a -> a -> LookupMap a
mapAdd (LookupMap es) e = LookupMap (e:es)

-- |Add supplied key-value pair to the lookup map,
--  only if the key value is not already present.
--
mapAddIfNew :: (LookupEntryClass a k v) =>
    LookupMap a -> a -> LookupMap a
mapAddIfNew emap e = if mapContains emap (entryKey e)
                        then emap
                        else mapAdd emap e

-- |Delete the first occurrence of the key from the lookup map.
--
--  If the key does not exist in the map then no change is made.
--
mapDelete :: (LookupEntryClass a k v) =>
    LookupMap a -> k -> LookupMap a
mapDelete l@(LookupMap []) _ = l
mapDelete (LookupMap (e:es)) k
    | k == entryKey e   = LookupMap es
    | otherwise         = mapAdd more e where
        more = mapDelete (LookupMap es) k

-- |Delete all occurrences of the key from the lookup map.
--
mapDeleteAll :: (LookupEntryClass a k v) =>
    LookupMap a -> k -> LookupMap a
mapDeleteAll l@(LookupMap []) _ = l
mapDeleteAll (LookupMap (e:es)) k =
    let more = mapDeleteAll (LookupMap es) k
    in if entryKey e == k then more else mapAdd more e
        
-- |Compare two lookup maps for equality.
--
--  Two maps are equal if they have the same set of keys, and if
--  each key maps to an equivalent value. This is only guaranteed
--  if the maps do not contain duplicate entries.
--
mapEq :: (LookupEntryClass a k v, Eq v) =>
    LookupMap a -> LookupMap a -> Bool
mapEq es1 es2 =
    ks1 `equiv` ks2 &&
    and [ mapFindMaybe k es1 == mapFindMaybe k es2 | k <- ks1 ]
    where
        ks1 = mapKeys es1
        ks2 = mapKeys es2

-- |Return the list of distinct keys in a supplied LookupMap
--
mapKeys :: (LookupEntryClass a k v) =>
    LookupMap a -> [k]
mapKeys = L.nub . gLM . fmap entryKey

-- |Return list of distinct values in a supplied LookupMap
--
mapVals :: (Eq v, LookupEntryClass a k v) =>
    LookupMap a -> [v]
mapVals = L.nub . gLM . fmap entryVal

-- |Merge two lookup maps, ensuring that if the same key appears
--  in both maps it is associated with the same value.
--
mapMerge :: (LookupEntryClass a k v, Eq a, Show a, Ord k) =>
    LookupMap a -> LookupMap a -> LookupMap a
mapMerge a b = LookupMap $ on merge (L.sortBy keyOrder . gLM) a b
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
