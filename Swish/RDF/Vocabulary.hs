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
--  Portability :  H98
--
--  This module defines some commonly used vocabulary terms,
--  using the 'Namespace' and 'ScopedName' data types.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary
    ( namespaceNull
    , namespaceRDF
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

import Data.Char (toLower)

------------------------------------------------------------
--  Define some common namespace values
------------------------------------------------------------

namespaceNull :: Namespace
namespaceNull
    = Namespace "" ""

namespaceRDF :: Namespace
namespaceRDF    =
    Namespace   "rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

namespaceRDFS :: Namespace
namespaceRDFS   =
    Namespace   "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"

namespaceRDFD :: Namespace
namespaceRDFD   =
    Namespace   "rdfd"  "http://id.ninebynine.org/2003/rdfext/rdfd#"

namespaceOWL :: Namespace
namespaceOWL    =
    Namespace   "owl"   "http://www.w3.org/2002/07/owl#"

namespaceXSD :: Namespace
namespaceXSD    =
    Namespace   "xsd"   "http://www.w3.org/2001/XMLSchema#"

namespaceXsdType :: String -> Namespace
namespaceXsdType dtname =
    Namespace   ("xsd_"++dtname)
                ("http://id.ninebynine.org/2003/XMLSchema/"++dtname++"#")

namespaceMATH :: Namespace
namespaceMATH   =
    Namespace   "math"  "http://www.w3.org/2000/10/swap/math#"

namespaceLOG :: Namespace
namespaceLOG    =
    Namespace   "log"   "http://www.w3.org/2000/10/swap/log#"
    -- Namespace   "log"   "http://www.w3.org/2000/10/swap/log.n3#"

namespaceDAML :: Namespace
namespaceDAML   =
    Namespace   "daml"  "http://www.daml.org/2000/10/daml-ont#"

namespaceDefault :: Namespace
namespaceDefault
    -- = Namespace "default" "#"
    = Namespace "default" "http://id.ninebynine.org/default/"

namespaceSwish :: Namespace
namespaceSwish
    = Namespace "swish" "http://id.ninebynine.org/2003/Swish/"

swishName :: String -> ScopedName
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

namespaceLang :: Namespace
namespaceLang
    = Namespace "lang" "http://id.ninebynine.org/2003/Swish/Lang/"
    -- To be replaced by urn:ietf:params:lang?

langName :: String -> ScopedName
langName = ScopedName namespaceLang . map toLower

langTag :: ScopedName -> String
langTag = snLocal

isLang :: ScopedName -> Bool
isLang sname = snScope sname == namespaceLang

------------------------------------------------------------
--  Define namespaces for RDF rules, axioms, etc
------------------------------------------------------------

scopeRDF :: Namespace
scopeRDF        =
    Namespace   "rs_rdf"   "http://id.ninebynine.org/2003/Ruleset/rdf#"

scopeRDFS :: Namespace
scopeRDFS       =
    Namespace   "rs_rdfs"  "http://id.ninebynine.org/2003/Ruleset/rdfs#"

scopeRDFD :: Namespace
scopeRDFD       =
    Namespace   "rs_rdfd"  "http://id.ninebynine.org/2003/Ruleset/rdfd#"

------------------------------------------------------------
--  Define some common vocabulary terms
------------------------------------------------------------

rdfDatatype   :: ScopedName
rdfDatatype   = ScopedName namespaceRDF  "datatype"

rdfResource   :: ScopedName
rdfResource   = ScopedName namespaceRDF  "resource"

rdfAbout      :: ScopedName
rdfAbout      = ScopedName namespaceRDF  "about"

rdfID         :: ScopedName
rdfID         = ScopedName namespaceRDF  "ID"

rdfType       :: ScopedName
rdfType       = ScopedName namespaceRDF  "type"

rdfFirst      :: ScopedName
rdfFirst      = ScopedName namespaceRDF  "first"

rdfRest       :: ScopedName
rdfRest       = ScopedName namespaceRDF  "rest"

rdfNil        :: ScopedName
rdfNil        = ScopedName namespaceRDF  "nil"

rdfXMLLiteral :: ScopedName
rdfXMLLiteral = ScopedName namespaceRDF  "XMLLiteral"

rdfsMember    :: ScopedName
rdfsMember    = ScopedName namespaceRDFS "member"

rdfdGeneralRestriction :: ScopedName
rdfdGeneralRestriction = ScopedName namespaceRDFD "GeneralRestriction"

rdfdOnProperties       :: ScopedName
rdfdOnProperties       = ScopedName namespaceRDFD "onProperties"

rdfdConstraint         :: ScopedName
rdfdConstraint         = ScopedName namespaceRDFD "constraint"

rdfdMaxCardinality     :: ScopedName
rdfdMaxCardinality     = ScopedName namespaceRDFD "maxCardinality"

xsdType                :: String -> ScopedName
xsdType                = ScopedName namespaceXSD

xsdString              :: ScopedName
xsdString              = xsdType "string"

xsdBoolean             :: ScopedName
xsdBoolean             = xsdType "boolean"

xsdDecimal             :: ScopedName
xsdDecimal             = xsdType "decimal"

xsdInteger             :: ScopedName
xsdInteger             = xsdType "integer"

xsdNonNegInteger      :: ScopedName
xsdNonNegInteger      = xsdType "nonNegativeInteger"

xsdNonPosInteger      :: ScopedName
xsdNonPosInteger      = xsdType "nonPositiveInteger"

xsdPosInteger         :: ScopedName
xsdPosInteger         = xsdType "positiveInteger"

xsdNegInteger         :: ScopedName
xsdNegInteger         = xsdType "negativeInteger"

xsdFloat               :: ScopedName
xsdFloat               = xsdType "float"

xsdDouble              :: ScopedName
xsdDouble              = xsdType "double"

xsdDate, xsdDateTime :: ScopedName
xsdDate = xsdType "date"
xsdDateTime = xsdType "dateTime"

owlSameAs              :: ScopedName
owlSameAs              = ScopedName namespaceOWL  "sameAs"

logImplies             :: ScopedName
logImplies             = ScopedName namespaceLOG "implies"

defaultBase            :: ScopedName
defaultBase            = ScopedName namespaceDefault "base"

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
