--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFDatatype
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98 + existential types
--
--  This module defines the structures used by Swish to represent and
--  manipulate RDF datatypes.
--
--------------------------------------------------------------------------------

module Swish.RDF.RDFDatatype
    ( RDFDatatype
    , RDFDatatypeVal
    , RDFDatatypeMod
    , RDFModifierFn, RDFApplyModifier
    , makeRdfDtOpenVarBindingModify, makeRdfDtOpenVarBindingModifiers
    , applyRDFDatatypeMod
    , RDFDatatypeSub
    , fromRDFLabel, toRDFLabel, makeDatatypedLiteral
    )
where

import Swish.RDF.RDFGraph
    ( RDFLabel(..)
    , isDatatyped
    , getLiteralText
    , RDFGraph
    )

import Swish.RDF.RDFVarBinding
    ( RDFVarBinding, RDFOpenVarBindingModify )

import Swish.RDF.Datatype
    ( Datatype -- , typeName, typeRules
    , DatatypeVal(..)
    , DatatypeMap(..)
    , DatatypeMod(..), ModifierFn
    , ApplyModifier
    , DatatypeSub(..)
    )

import Swish.Utils.Namespace
    ( ScopedName(..) )

import Swish.RDF.VarBinding
    ( VarBindingModify(..) )

import Data.Maybe
    ( fromMaybe, isJust, fromJust )

import Control.Monad
    ( liftM )

------------------------------------------------------------
--  Specialize datatype framework types for use with RDF
------------------------------------------------------------

-- |RDF datatype wrapper used with RDF graph values
--
type RDFDatatype = Datatype RDFGraph RDFLabel RDFLabel

-- |RDF datatype value used with RDF graph values
--
type RDFDatatypeVal vt = DatatypeVal RDFGraph vt RDFLabel RDFLabel

-- |RDF datatype modifier used with RDF graph values
--
type RDFDatatypeMod vt = DatatypeMod vt RDFLabel RDFLabel

-- |Describe a subtype/supertype relationship between a pair
--  of RDF datatypes.
--
type RDFDatatypeSub supvt subvt = DatatypeSub RDFGraph RDFLabel RDFLabel supvt subvt

-- |RDF value modifier function type
--
--  This indicates a modifier function that operates on 'RDFLabel' values.
--
type RDFModifierFn = ModifierFn RDFLabel

-- |RDF value modifier application function type
--
--  This indicates a function that applies RDFModifierFn functions.
--
type RDFApplyModifier = ApplyModifier RDFLabel RDFLabel

--------------------------------------------------------------
--  Functions for creating datatype variable binding modifiers
--------------------------------------------------------------

-- |Create an 'RDFOpenVarBindingModify' value.
--
--  [@dtval@]   is an 'RDFDatatype' value containing details of the datatype
--          for which a variable binding modifier is created.
--
--  [@dtmod@]   is the data value modifier value that defines the calculations
--          that are used to implement a variable binding modifier.
--
--  The key purpose of this function is to lift the supplied
--  variable constraint functions from operating on data values directly
--  to a corresponding list of functions that operate on values contained
--  in RDF graph labels (i.e. RDF literal nodes).  It also applies
--  node type checking, such that if the actual RDF nodes supplied do
--  not contain appropriate values then the variable binding is not
--  accepted.
--
makeRdfDtOpenVarBindingModify ::
    RDFDatatypeVal vt -> RDFDatatypeMod vt -> RDFOpenVarBindingModify
makeRdfDtOpenVarBindingModify dtval dtmod =
    dmAppf dtmod (dmName dtmod) $ map (makeRDFModifierFn dtval) (dmModf dtmod)

-- |Create all RDFOpenVarBindingModify values for a given datatype value.
--  See 'makeRdfDtOpenVarBindingModify'.
--
--  [@dtval@]   is an 'RDFDatatype' value containing details of the datatype
--          for which variable binding modifiers are created.
--
makeRdfDtOpenVarBindingModifiers ::
    RDFDatatypeVal vt -> [RDFOpenVarBindingModify]
makeRdfDtOpenVarBindingModifiers dtval =
    map (makeRdfDtOpenVarBindingModify dtval) (tvalMod dtval)

-- |Apply a datatype modifier using supplied RDF labels to a supplied
--  RDF variable binding.
--
applyRDFDatatypeMod ::
    RDFDatatypeVal vt -> RDFDatatypeMod vt -> [RDFLabel] -> [RDFVarBinding]
    -> [RDFVarBinding]
applyRDFDatatypeMod dtval dtmod lbs =
    vbmApply (makeRdfDtOpenVarBindingModify dtval dtmod lbs)

-- |Given details of a datatype and a single value constraint function,
--  return a new constraint function that operates on 'RDFLabel' values.
--
--  The returned constraint function incorporates checks for appropriately
--  typed literal nodes, and returns similarly typed literal nodes.
--
makeRDFModifierFn ::
    RDFDatatypeVal vt -> ModifierFn vt -> RDFModifierFn
makeRDFModifierFn dtval fn ivs =
    let
        ivals = mapM (rdfNodeExtract dtval) ivs
        ovals | isJust ivals = fn (fromJust ivals)
              | otherwise    = []
    in
        fromMaybe [] $ mapM (rdfNodeInject dtval) ovals

-- |Extract datatyped value from 'RDFLabel' value, or return @Nothing@.
--
rdfNodeExtract :: RDFDatatypeVal vt -> RDFLabel -> Maybe vt
rdfNodeExtract dtval node
    | isDatatyped dtname node = mapL2V dtmap $ getLiteralText node
    | otherwise               = Nothing
    where
        dtname = tvalName dtval
        dtmap  = tvalMap  dtval

-- |Return new RDF literal node with a representation of the supplied
--  value, or @Nothing@.
--
rdfNodeInject :: RDFDatatypeVal vt -> vt -> Maybe RDFLabel
rdfNodeInject dtval val = maybeNode valstr
    where
        valstr = mapV2L (tvalMap  dtval) val
        maybeNode Nothing    = Nothing
        maybeNode (Just str) = Just $ Lit str (Just (tvalName dtval))

------------------------------------------------------------
--  Helpers to map between datatype values and RDFLabels
------------------------------------------------------------

fromRDFLabel ::
    RDFDatatypeVal vt -> RDFLabel -> Maybe vt
fromRDFLabel dtv lab
    | isDatatyped dtnam lab = mapL2V dtmap $ getLiteralText lab
    | otherwise             = Nothing
    where
        dtnam = tvalName dtv
        dtmap = tvalMap dtv

toRDFLabel :: RDFDatatypeVal vt -> vt -> Maybe RDFLabel
toRDFLabel dtv =
    liftM (makeDatatypedLiteral dtnam) . mapV2L dtmap
    where
        dtnam = tvalName dtv
        dtmap = tvalMap dtv

makeDatatypedLiteral :: ScopedName -> String -> RDFLabel
makeDatatypedLiteral dtnam strval =
    Lit strval (Just dtnam)

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
