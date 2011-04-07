--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  BuiltInRules
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module collects references and provides access to all of the
--  rulesets, variable binding modifiers and variable binding filters
--  built in to Swish.
--
--------------------------------------------------------------------------------

module Swish.RDF.BuiltInRules
    ( findRDFOpenVarBindingModifier
    , rdfRulesetMap
    , allRulesets, allDatatypeRulesets
    )
where

import Swish.RDF.BuiltInDatatypes
    ( allDatatypes )

import Swish.RDF.RDFVarBinding
    ( RDFOpenVarBindingModify
    , rdfVarBindingUriRef, rdfVarBindingBlank
    , rdfVarBindingLiteral
    , rdfVarBindingUntypedLiteral, rdfVarBindingTypedLiteral
    , rdfVarBindingXMLLiteral, rdfVarBindingDatatyped
    , rdfVarBindingMemberProp
    )

import Swish.RDF.RDFRuleset
    ( RDFRuleset, RDFRulesetMap )

import Swish.RDF.RDFProofContext
    ( rulesetRDF
    , rulesetRDFS
    , rulesetRDFD )

import Swish.RDF.VarBinding
    ( nullVarBindingModify
    , makeVarFilterModify
    , varFilterEQ, varFilterNE
    )

import Swish.RDF.Datatype
    ( typeRules
    , typeMkModifiers
    )

import Swish.Utils.LookupMap
    ( LookupMap(..)
    , mapFindMaybe
    )

import Swish.Utils.Namespace
    ( ScopedName(..) )

------------------------------------------------------------
--  Declare variable binding filters list
------------------------------------------------------------

-- |List of rdfOpenVarBindingModify values for predefined filters
--
rdfVarBindingFilters :: [RDFOpenVarBindingModify]
rdfVarBindingFilters =
    [ filter1 rdfVarBindingUriRef
    , filter1 rdfVarBindingBlank
    , filter1 rdfVarBindingLiteral
    , filter1 rdfVarBindingUntypedLiteral
    , filter1 rdfVarBindingTypedLiteral
    , filter1 rdfVarBindingXMLLiteral
    , filter1 rdfVarBindingMemberProp
    , filter2 rdfVarBindingDatatyped
    -- , filterN nullVarBindingModify
    , filter2 varFilterEQ
    , filter2 varFilterNE
    ]
    where
        filter1 f lbs = makeVarFilterModify $ f (head lbs)
        filter2 f lbs = makeVarFilterModify $ f (head lbs) (lbs!!1)
        -- filterN f lbs = makeVarFilterModify $ f ...

------------------------------------------------------------
--  Declare variable binding modifiers map
------------------------------------------------------------

rdfVarBindingModifiers :: [RDFOpenVarBindingModify]
rdfVarBindingModifiers =
    [ nullVarBindingModify
    ]

------------------------------------------------------------
--  Find a named built-in OpenVarBindingModifier
------------------------------------------------------------

allOpenVarBindingModify :: [RDFOpenVarBindingModify]
allOpenVarBindingModify =
    rdfVarBindingFilters    ++
    rdfVarBindingModifiers  ++
    dtsVarBindingModifiers

dtsVarBindingModifiers :: [RDFOpenVarBindingModify]
-- dtsVarBindingModifiers = concatMap dtVarBindingModifiers allDatatypes
dtsVarBindingModifiers = concatMap typeMkModifiers allDatatypes

{-
dtVarBindingModifiers dtval =
    map (makeRdfDtOpenVarBindingModify dtval) (tvalMod dtval)
-}

findRDFOpenVarBindingModifier :: ScopedName -> Maybe RDFOpenVarBindingModify
findRDFOpenVarBindingModifier nam =
    mapFindMaybe nam (LookupMap allOpenVarBindingModify)

------------------------------------------------------------
--  Lookup map for built-in rulesets
------------------------------------------------------------

rdfRulesetMap :: RDFRulesetMap
rdfRulesetMap = LookupMap allRulesets

allRulesets :: [RDFRuleset]
allRulesets =
    [ rulesetRDF
    , rulesetRDFS
    , rulesetRDFD
    ]
    ++ allDatatypeRulesets

allDatatypeRulesets :: [RDFRuleset]
allDatatypeRulesets = map typeRules allDatatypes

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
