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
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines a Labelled Directed Graph and Label classes,
--  and the Arc datatype.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Define LDGraph, arc and related classes and types
------------------------------------------------------------

module Swish.HaskellRDF.GraphClass
    ( LDGraph(..), replaceArcs
    , Label(..)
    , Arc(..), arcSubj, arcPred, arcObj, arc, arcToTriple, arcFromTriple
    , Selector
    , hasLabel, arcLabels
    )
where

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.List (union, (\\))

--------------------------------
--  Labelled Directed Graph class
--------------------------------
--
--  Minimum required implementation:  setArcs, getArcs
--
--  NOTE:  I wanted to declare this as a subclass of Functor, but
--  the constraint on the label type seems to prevent that.
--  So I've just declared specific instances to be Functors.
class (Eq (lg lb), Eq lb ) => LDGraph lg lb
    where
    --  empty graph
    --  emptyGr     :: lg lb    [[[TODO?]]]
    --  component-level operations
    setArcs     :: [Arc lb] -> lg lb -> lg lb       -- setarcs [arcs] in g2 -> g3
    getArcs     :: lg lb -> [Arc lb]                -- g1 -> [arcs]
    --  extract arcs from a graph
    extract     :: Selector lb -> lg lb -> lg lb    -- select f1 from g2 -> g3
    extract sel = update (filter sel)
    --  graph-level operations
    add         :: lg lb -> lg lb -> lg lb          -- g1 + g2 -> g3
    add    addg = update (union (getArcs addg))
    delete      :: lg lb -> lg lb -> lg lb          -- g2 - g1 -> g3
    delete delg = update (\\ getArcs delg)
    --  enumerate distinct labels contained in a graph
    labels      :: lg lb -> [lb]      -- g1 -> [labels]
    labels g    = foldl union [] (map arcLabels (getArcs g))
    --  enumerate distinct labels contained in a graph
    nodes       :: lg lb -> [lb]      -- g1 -> [labels]
    nodes g     = foldl union [] (map arcNodes (getArcs g))
    --  test for graph containment in another
    containedIn :: lg lb -> lg lb -> Bool           -- g1 <= g2?
    -- g1 update arcs in a graph using a supplied function:
    update      :: ( [Arc lb] -> [Arc lb] ) -> lg lb -> lg lb
    update f g  = setArcs ( f (getArcs g) ) g

-- |Function to replace arcs in a graph with a given list of arcs
replaceArcs :: (LDGraph lg lb) => lg lb -> [Arc lb] -> lg lb
replaceArcs gr as = update (const as) gr

---------------
--  Label class
---------------
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
    labelIsVar  :: lb -> Bool           -- does this node have a variable binding?
    labelHash   :: Int -> lb -> Int     -- calculate hash of label using supplied seed
    getLocal    :: lb -> String         -- extract local id from variable node
    makeLabel   :: String -> lb         -- make label value given local id
    -- compare     :: lb -> lb -> Ordering
    -- compare l1 l2 = compare (show l1) (show l2)

------------
--  Arc type
------------

data Arc lb = Arc { asubj, apred, aobj :: lb }
    deriving (Eq, Functor, F.Foldable, T.Traversable)

arcSubj :: Arc lb -> lb
arcSubj = asubj

arcPred :: Arc lb -> lb
arcPred = apred

arcObj :: Arc lb -> lb
arcObj = aobj

arc :: lb -> lb -> lb -> Arc lb
arc = Arc

arcToTriple :: Arc lb -> (lb,lb,lb)
arcToTriple a = (asubj a,apred a,aobj a)

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

  (Arc s1 p1 o1) <= (Arc s2 p2 o2)
    | s1 /= s2 = s1 <= s2
    | p1 /= p2 = p1 <= p2
    | otherwise = o1 <= o2

instance (Show lb) => Show (Arc lb) where
    show (Arc lb1 lb2 lb3) =
        "("++ show lb1 ++","++ show lb2 ++","++ show lb3 ++")"

type Selector lb = Arc lb -> Bool

hasLabel :: (Eq lb) => lb -> Arc lb -> Bool
hasLabel lbv (Arc lb1 lb2 lb3) = lbv `elem` [lb1, lb2, lb3]

arcLabels :: Arc lb -> [lb]
arcLabels (Arc lb1 lb2 lb3) = [lb1,lb2,lb3]

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
