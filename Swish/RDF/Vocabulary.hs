{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Vocabulary
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms,
--  using the 'Namespace' and 'ScopedName' data types.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceOWL
    , namespaceXSD
    , namespaceXsdType
    , namespaceMATH
    , namespaceLOG
    , namespaceDAML
    , namespaceDefault
    , namespaceSwish, swishName
    , namespaceLang,  langName, langTag, isLang
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    , rdfDatatype, rdfResource, rdfAbout, rdfID
    , rdfType
    , rdfFirst, rdfRest, rdfNil, rdfXMLLiteral
    , rdfsMember
    , rdfdGeneralRestriction
    , rdfdOnProperties, rdfdConstraint, rdfdMaxCardinality
    , owlSameAs, logImplies
    , xsdType, xsdString, xsdBoolean
    , xsdDecimal, xsdInteger
    , xsdNonNegInteger, xsdNonPosInteger, xsdPosInteger, xsdNegInteger
    , xsdFloat, xsdDouble
    , xsdDate, xsdDateTime
    , defaultBase
    )
where

import Swish.Utils.Namespace (Namespace(..), ScopedName(..))

import Data.Monoid (mappend, mconcat)
import Data.Maybe (fromMaybe)
import Network.URI (parseURI)

import qualified Data.Text as T

------------------------------------------------------------
--  Define some common namespace values
------------------------------------------------------------

toNS :: T.Text -> T.Text -> Namespace
toNS p utxt = 
  let ustr = T.unpack utxt
      uri = fromMaybe (error ("Unable to convert " ++ ustr ++ " to a URI")) $
            parseURI ustr
  in Namespace (Just p) uri

namespaceXsdType :: T.Text -> Namespace
namespaceXsdType dtn = toNS ("xsd_" `mappend` dtn)
                       (mconcat ["http://id.ninebynine.org/2003/XMLSchema/", dtn, "#"])

namespaceRDF :: Namespace
namespaceRDFS :: Namespace
namespaceRDFD :: Namespace
namespaceOWL :: Namespace
namespaceXSD :: Namespace
namespaceMATH :: Namespace
namespaceLOG :: Namespace
namespaceDAML :: Namespace
namespaceSwish :: Namespace
namespaceDefault :: Namespace
namespaceLang :: Namespace

namespaceRDF     = toNS "rdf"     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
namespaceRDFS    = toNS "rdfs"    "http://www.w3.org/2000/01/rdf-schema#"
namespaceRDFD    = toNS "rdfd"    "http://id.ninebynine.org/2003/rdfext/rdfd#"
namespaceOWL     = toNS "owl"     "http://www.w3.org/2002/07/owl#"
namespaceXSD     = toNS "xsd"     "http://www.w3.org/2001/XMLSchema#"
namespaceMATH    = toNS "math"    "http://www.w3.org/2000/10/swap/math#"
namespaceLOG     = toNS "log"     "http://www.w3.org/2000/10/swap/log#"
namespaceDAML    = toNS "daml"    "http://www.daml.org/2000/10/daml-ont#"
namespaceSwish   = toNS "swish"   "http://id.ninebynine.org/2003/Swish/"
namespaceLang    = toNS "lang"    "http://id.ninebynine.org/2003/Swish/Lang/" -- To be replaced by urn:ietf:params:lang?
namespaceDefault = toNS "default" "http://id.ninebynine.org/default/"

swishName :: T.Text -> ScopedName
swishName = ScopedName namespaceSwish

-----------------------------------------------------------
--  Language tags
------------------------------------------------------------
--
--  Note:  simple language tag URIs may be abbreviated as lang:tag,
--  but if the tag contains a hyphen, this would not be valid QName
--  form in Notation3, even though it is a valid QName component.
--  Fortunately, they do not currently need to appear in Notation3 as
--  distinct labels (but future developments may change that).

langName :: T.Text -> ScopedName
langName = ScopedName namespaceLang . T.toLower

langTag :: ScopedName -> T.Text
langTag = snLocal

isLang :: ScopedName -> Bool
isLang sname = snScope sname == namespaceLang

------------------------------------------------------------
--  Define namespaces for RDF rules, axioms, etc
------------------------------------------------------------

scopeRDF, scopeRDFS, scopeRDFD :: Namespace

scopeRDF  = toNS "rs_rdf"   "http://id.ninebynine.org/2003/Ruleset/rdf#"
scopeRDFS = toNS "rs_rdfs"  "http://id.ninebynine.org/2003/Ruleset/rdfs#"
scopeRDFD = toNS "rs_rdfd"  "http://id.ninebynine.org/2003/Ruleset/rdfd#"

------------------------------------------------------------
--  Define some common vocabulary terms
------------------------------------------------------------

toRDF, toRDFS, toRDFD :: T.Text -> ScopedName
toRDF  = ScopedName namespaceRDF
toRDFS = ScopedName namespaceRDFS
toRDFD = ScopedName namespaceRDFD

rdfDatatype   :: ScopedName
rdfResource   :: ScopedName
rdfAbout      :: ScopedName
rdfID         :: ScopedName
rdfType       :: ScopedName
rdfFirst      :: ScopedName
rdfRest       :: ScopedName
rdfNil        :: ScopedName
rdfXMLLiteral :: ScopedName

rdfDatatype   = toRDF "datatype"
rdfResource   = toRDF "resource"
rdfAbout      = toRDF "about"
rdfID         = toRDF "ID"
rdfType       = toRDF "type"
rdfFirst      = toRDF "first"
rdfRest       = toRDF "rest"
rdfNil        = toRDF "nil"
rdfXMLLiteral = toRDF "XMLLiteral"

rdfsMember    :: ScopedName
rdfsMember    = toRDFS "member"

rdfdGeneralRestriction :: ScopedName
rdfdOnProperties       :: ScopedName
rdfdConstraint         :: ScopedName
rdfdMaxCardinality     :: ScopedName

rdfdGeneralRestriction = toRDFD "GeneralRestriction"
rdfdOnProperties       = toRDFD "onProperties"
rdfdConstraint         = toRDFD "constraint"
rdfdMaxCardinality     = toRDFD "maxCardinality"

xsdType             :: T.Text -> ScopedName
xsdType             = ScopedName namespaceXSD

xsdString           :: ScopedName
xsdString           = xsdType "string"

xsdBoolean          :: ScopedName
xsdBoolean          = xsdType "boolean"

xsdDecimal          :: ScopedName
xsdDecimal          = xsdType "decimal"

xsdInteger          :: ScopedName
xsdInteger          = xsdType "integer"

xsdNonNegInteger   :: ScopedName
xsdNonNegInteger   = xsdType "nonNegativeInteger"

xsdNonPosInteger   :: ScopedName
xsdNonPosInteger   = xsdType "nonPositiveInteger"

xsdPosInteger      :: ScopedName
xsdPosInteger      = xsdType "positiveInteger"

xsdNegInteger      :: ScopedName
xsdNegInteger      = xsdType "negativeInteger"

xsdFloat            :: ScopedName
xsdFloat            = xsdType "float"

xsdDouble           :: ScopedName
xsdDouble           = xsdType "double"

xsdDate, xsdDateTime :: ScopedName
xsdDate = xsdType "date"
xsdDateTime = xsdType "dateTime"

owlSameAs   :: ScopedName
owlSameAs   = ScopedName namespaceOWL  "sameAs"

logImplies  :: ScopedName
logImplies  = ScopedName namespaceLOG "implies"

defaultBase :: ScopedName
defaultBase = ScopedName namespaceDefault "base"

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
