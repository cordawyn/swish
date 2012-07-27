{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  GraphMem
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses
--
--  This module defines a simple memory-based graph instance.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Simple labelled directed graph value
------------------------------------------------------------

module Swish.GraphMem
    ( GraphMem(..)
    , setArcs, getArcs, add, delete, extract, labels
    , LabelMem(..)
    , labelIsVar, labelHash
      -- For debug/test:
    , matchGraphMem
    ) where

import Swish.GraphClass
import Swish.GraphMatch

import Data.Hashable (Hashable(..), combine)
import Data.Ord (comparing)

import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- | Simple memory-based graph type. 

data GraphMem lb = GraphMem { arcs :: [Arc lb] }
                   deriving (Functor, F.Foldable, T.Traversable)
                            
instance (Label lb) => LDGraph GraphMem lb where
    getArcs      = arcs
    setArcs as g = g { arcs=as }
    -- gmap f g = g { arcs = (map $ fmap f) (arcs g) }

instance (Label lb) => Eq (GraphMem lb) where
    (==) = graphEq

instance (Label lb) => Show (GraphMem lb) where
    show = graphShow

graphShow   :: (Label lb) => GraphMem lb -> String
graphShow g = "Graph:" ++ foldr ((++) . ("\n    " ++) . show) "" (arcs g)

{-
toGraph :: (Label lb) => [Arc lb] -> GraphMem lb
toGraph as = GraphMem { arcs=nub as }
-}

-- |  Return Boolean graph equality

graphEq :: (Label lb) => GraphMem lb -> GraphMem lb -> Bool
graphEq g1 g2 = fst ( matchGraphMem g1 g2 )

-- | GraphMem matching function accepting GraphMem value and returning
--  node map if successful
--
matchGraphMem ::
  (Label lb)
  => GraphMem lb 
  -> GraphMem lb
  -> (Bool,LabelMap (ScopedLabel lb))
  -- ^ if the first element is @True@ then the second value is a label
  --   map that maps each label to an equivalence-class identifier,
  --   otherwise `emptyMap`.
  --
matchGraphMem g1 g2 =
    let
        gs1     = arcs g1
        gs2     = arcs g2
        matchable l1 l2
            | labelIsVar l1 && labelIsVar l2 = True
            | labelIsVar l1 || labelIsVar l2 = False
            | otherwise                      = l1 == l2
    in
        graphMatch matchable gs1 gs2

{-
-- |  Return bijection between two graphs, or empty list
graphBiject :: (Label lb) => GraphMem lb -> GraphMem lb -> [(lb,lb)]
graphBiject g1 g2 = if null lmap then [] else zip (sortedls g1) (sortedls g2)
    where
        lmap        = graphMatch g1 g2
        sortedls g  = map snd $
                      (sortBy indexComp) $
                      equivalenceClasses (graphLabels $ arcs g) lmap
        classComp ec1 ec2 = indexComp (classIndexVal ec1) (classIndexVal ec2)
        indexComp (g1,v1) (g2,v2)
            | g1 == g2  = compare v1 v2
            | otherwise = compare g1 g2
-}

-- |  Minimal graph label value - for testing

data LabelMem
    = LF String
    | LV String

instance Hashable LabelMem where
  hash (LF l) = 1 `hashWithSalt` l
  hash (LV l) = 2 `hashWithSalt` l
  hashWithSalt salt (LF l) = salt `combine` 1 `hashWithSalt` l
  hashWithSalt salt (LV l) = salt `combine` 2 `hashWithSalt` l

instance Label LabelMem where
    labelIsVar (LV _)   = True
    labelIsVar _        = False
    getLocal   (LV loc) = loc
    getLocal   lab      = error "getLocal of non-variable label: " ++ show lab
    makeLabel           = LV 
    labelHash = hashWithSalt

instance Eq LabelMem where
    (LF l1) == (LF l2)  = l1 == l2
    (LV l1) == (LV l2)  = l1 == l2
    _ == _              = False

instance Show LabelMem where
    show (LF l1)        = '!' : l1
    show (LV l2)        = '?' : l2

instance Ord LabelMem where
    compare = comparing show 

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
