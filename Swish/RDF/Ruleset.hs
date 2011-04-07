{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Ruleset
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  MultiParamTypeClasses
--
--  This module defines a ruleset data type, used to collect information
--  about a ruleset that may contribute torwards inferences in RDF;
--  e.g. RDF and RDFS are rulesets.
--
--  A ruleset consists of a namespace, a collection of axioms and
--  a collection of rules.
--
--------------------------------------------------------------------------------

module Swish.RDF.Ruleset
    ( Ruleset(..), RulesetMap
    , makeRuleset, getRulesetNamespace, getRulesetAxioms, getRulesetRules
    , getRulesetAxiom, getRulesetRule
    , getContextAxiom, getMaybeContextAxiom
    , getContextRule,  getMaybeContextRule
    )
where

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    )

import Swish.RDF.Rule
    ( Formula(..), Rule(..) )

import Swish.Utils.LookupMap
    ( LookupEntryClass(..), LookupMap(..)
    , mapFindMaybe
    )

import Data.Maybe
    ( fromMaybe, listToMaybe, mapMaybe )

-- | Ruleset, having namespace, axioms and rules

data Ruleset ex = Ruleset
    { rsNamespace :: Namespace
    , rsAxioms    :: [Formula ex]
    , rsRules     :: [Rule ex]
    }

instance Eq (Ruleset ex) where
    r1 == r2 = rsNamespace r1 == rsNamespace r2

instance LookupEntryClass (Ruleset ex) Namespace (Ruleset ex)
    where
        keyVal   r@(Ruleset k _ _) = (k,r)
        newEntry (_,r)             = r

type RulesetMap ex = LookupMap (Ruleset ex)

makeRuleset :: Namespace -> [Formula ex] -> [Rule ex] -> Ruleset ex
makeRuleset nsp fms rls = Ruleset
    { rsNamespace = nsp
    , rsAxioms    = fms
    , rsRules     = rls
    }

getRulesetNamespace :: Ruleset ex -> Namespace
getRulesetNamespace = rsNamespace

getRulesetAxioms :: Ruleset ex -> [Formula ex]
getRulesetAxioms = rsAxioms

getRulesetRules :: Ruleset ex -> [Rule ex]
getRulesetRules = rsRules

-- | Find a named axiom or rule in a ruleset or proof context

getRulesetAxiom :: ScopedName -> Ruleset ex -> Maybe (Formula ex)
getRulesetAxiom nam rset =
    mapFindMaybe nam (LookupMap (getRulesetAxioms rset))
    -- listToMaybe $ filter ( (matchName nam) . formName ) $ getRulesetAxioms rset

getRulesetRule :: ScopedName -> Ruleset ex -> Maybe (Rule ex)
getRulesetRule nam rset =
    mapFindMaybe nam (LookupMap (getRulesetRules rset))
    -- listToMaybe $ filter ( (matchName nam) . ruleName ) $ getRulesetRules rset

getContextAxiom :: ScopedName -> Formula ex -> [Ruleset ex] -> Formula ex
getContextAxiom nam def rsets = fromMaybe def (getMaybeContextAxiom nam rsets)
    {-
    foldr (flip fromMaybe) def $ map (getRulesetAxiom nam) rsets
    -}

getMaybeContextAxiom :: ScopedName -> [Ruleset ex] -> Maybe (Formula ex)
getMaybeContextAxiom nam rsets =
    listToMaybe $ mapMaybe (getRulesetAxiom nam) rsets

getContextRule :: ScopedName -> Rule ex -> [Ruleset ex] -> Rule ex
getContextRule nam def rsets = fromMaybe def (getMaybeContextRule nam rsets)
    {-
    foldr (flip fromMaybe) def $ map (getRulesetRule nam) rsets
    -}

getMaybeContextRule :: ScopedName -> [Ruleset ex] -> Maybe (Rule ex)
getMaybeContextRule nam rsets =
    listToMaybe $ mapMaybe (getRulesetRule nam) rsets

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
