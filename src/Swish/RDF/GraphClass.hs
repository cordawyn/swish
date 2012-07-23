{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphClass
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses
--
--  This module defines a Labelled Directed Graph and Label classes,
--  and the Arc datatype.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Define LDGraph, arc and related classes and types
------------------------------------------------------------

module Swish.RDF.GraphClass
    ( LDGraph(..), replaceArcs
    , Label(..)
    , Arc(..), arcSubj, arcPred, arcObj, arc, arcToTriple, arcFromTriple
    , Selector
    , hasLabel, arcLabels -- , arcNodes
    )
where

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.Hashable (Hashable(..))

import Data.List (foldl', union, (\\))

--  NOTE:  I wanted to declare this as a subclass of Functor, but
--  the constraint on the label type seems to prevent that.
--  So I've just declared specific instances to be Functors.
--

{-|
Labelled Directed Graph class

Minimum required implementation:  `setArcs` and `getArcs`.
-}
class (Eq (lg lb), Eq lb ) => LDGraph lg lb
    where
    --  empty graph
    --  emptyGr     :: lg lb    [[[TODO?]]]
    --  component-level operations
      
    -- | Replace the existing arcs in the graph.
    setArcs     :: [Arc lb] -> lg lb -> lg lb
    
    -- | Extract all the arcs from a graph
    getArcs     :: lg lb -> [Arc lb]
    
    -- | Extract those arcs that match the given `Selector`.
    extract     :: Selector lb -> lg lb -> lg lb
    extract sel = update (filter sel)
    
    -- | Add the two graphs
    add         :: lg lb -> lg lb -> lg lb
    add    addg = update (union (getArcs addg))
    
    -- | Remove those arcs in the first graph from the second
    -- graph
    delete :: lg lb  -- ^ g1
              -> lg lb -- ^ g2
              -> lg lb -- ^ g2 - g1 -> g3
    delete delg = update (\\ getArcs delg)
    
    -- | Enumerate the distinct labels contained in a graph;
    -- that is, any label that appears in the subject,
    -- predicate or object position of an `Arc`.
    labels      :: lg lb -> [lb]
    labels g    = foldl' union [] (map arcLabels (getArcs g))
    
    -- | Enumerate the distinct nodes contained in a graph;
    -- that is, any label that appears in the subject
    -- or object position of an `Arc`.
    nodes       :: lg lb -> [lb]
    nodes g     = foldl' union [] (map arcNodes (getArcs g))
    
    -- | Update the arcs in a graph using a supplied function.
    update      :: ( [Arc lb] -> [Arc lb] ) -> lg lb -> lg lb
    update f g  = setArcs ( f (getArcs g) ) g

{-
TODO:
  add a Monoid instance for LDGraph, so that we can remove the
  NSGraph instance in RDFGraph

  This means adding the emptyGr function to the interface
-}

-- |Function to replace arcs in a graph with a given list of arcs.
--
-- This is identical to @flip setArcs@ and so may be removed.
--    
replaceArcs :: (LDGraph lg lb) => lg lb -> [Arc lb] -> lg lb
replaceArcs = flip setArcs
-- replaceArcs gr as = update (const as) gr

-- | Label class
--
--  A label may have a fixed binding, which means that the label identifies (is) a
--  particular graph node, and different such labels are always distinct nodes.
--  Alternatively, a label may be unbound (variable), which means that it is a
--  placeholder for an unknown node label.  Unbound node labels are used as
--  graph-local identifiers for indicating when the same node appears in
--  several arcs.
--
--  For the purposes of graph-isomorphism testing, fixed labels are matched when they
--  are the same.  Variable labels may be matched with any other variable label.
--  Our definition of isomorphism (for RDF graphs) does not match variable labels
--  with fixed labels.

class (Eq lb, Show lb, Ord lb) => Label lb where
  
  -- | Does this node have a variable binding?
  labelIsVar  :: lb -> Bool           
    
  -- | Calculate the hash of the label using the supplied seed.
  labelHash   :: Int -> lb -> Int     
  
  -- could provide a default of 
  --   labelHash = hashWithSalt
  -- but this would then force a Hashable constraint
    
  -- | Extract the local id from a variable node.                 
  getLocal    :: lb -> String
    
  -- | Make a label value from a local id.  
  makeLabel   :: String -> lb
    
  -- compare     :: lb -> lb -> Ordering
  -- compare l1 l2 = compare (show l1) (show l2)

-- | Arc type

data Arc lb = Arc 
              { asubj :: lb  -- ^ The subject of the arc.
              , apred :: lb  -- ^ The predicate (property) of the arc.
              , aobj :: lb   -- ^ The object of the arc.
              }
            deriving (Eq, Functor, F.Foldable, T.Traversable)

instance (Hashable lb) => Hashable (Arc lb) where
  hash (Arc s p o) = hash s `hashWithSalt` p `hashWithSalt` o
  hashWithSalt salt (Arc s p o) = salt `hashWithSalt` s `hashWithSalt` p `hashWithSalt` o

-- | Return the subject of the arc.
arcSubj :: Arc lb -> lb
arcSubj = asubj

-- | Return the predicate (property) of the arc.
arcPred :: Arc lb -> lb
arcPred = apred

-- | Return the object of the arc.
arcObj :: Arc lb -> lb
arcObj = aobj

-- | Create an arc.
arc :: lb      -- ^ The subject of the arc.
       -> lb   -- ^ The predicate of the arc.
       -> lb   -- ^ The object of the arc.
       -> Arc lb
arc = Arc

-- | Convert an Arc into a tuple.
arcToTriple :: Arc lb -> (lb,lb,lb)
arcToTriple (Arc s p o) = (s, p, o)

-- | Create an Arc from a tuple.
arcFromTriple :: (lb,lb,lb) -> Arc lb
arcFromTriple (s,p,o) = Arc s p o

instance Ord lb => Ord (Arc lb) where
  compare (Arc s1 p1 o1) (Arc s2 p2 o2)
    | cs /= EQ = cs
    | cp /= EQ = cp
    | otherwise = co
    where
      cs = compare s1 s2
      cp = compare p1 p2
      co = compare o1 o2

  {- not needed
  (Arc s1 p1 o1) <= (Arc s2 p2 o2)
    | s1 /= s2 = s1 <= s2
    | p1 /= p2 = p1 <= p2
    | otherwise = o1 <= o2
  -}

instance (Show lb) => Show (Arc lb) where
    show (Arc lb1 lb2 lb3) =
        "("++ show lb1 ++","++ show lb2 ++","++ show lb3 ++")"

-- | Identify arcs.
type Selector lb = Arc lb -> Bool

hasLabel :: (Eq lb) => lb -> Arc lb -> Bool
hasLabel lbv lb = lbv `elem` arcLabels lb

-- | Return all the labels in an arc.
arcLabels :: Arc lb -> [lb]
arcLabels (Arc lb1 lb2 lb3) = [lb1,lb2,lb3]

-- | Return just the subject and object labels in the arc.
arcNodes :: Arc lb -> [lb]
arcNodes (Arc lb1 _ lb3) = [lb1,lb3]

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
