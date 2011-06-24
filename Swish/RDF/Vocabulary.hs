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
    , rdf_datatype, rdf_resource, rdf_about, rdf_ID
    , rdf_type
    , rdf_first, rdf_rest, rdf_nil, rdf_XMLLiteral
    , rdfs_member
    , rdfd_GeneralRestriction
    , rdfd_onProperties, rdfd_constraint, rdfd_maxCardinality
    , owl_sameAs, log_implies
    , xsd_type, xsd_string, xsd_boolean
    , xsd_decimal, xsd_integer
    , xsd_nonneg_integer, xsd_nonpos_integer, xsd_pos_integer, xsd_neg_integer
    , xsd_float, xsd_double
    , xsd_date, xsd_dateTime
    , default_base
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

rdf_datatype            :: ScopedName
rdf_datatype            = ScopedName namespaceRDF  "datatype"

rdf_resource            :: ScopedName
rdf_resource            = ScopedName namespaceRDF  "resource"

rdf_about               :: ScopedName
rdf_about               = ScopedName namespaceRDF  "about"

rdf_ID                  :: ScopedName
rdf_ID                  = ScopedName namespaceRDF  "ID"

rdf_type                :: ScopedName
rdf_type                = ScopedName namespaceRDF  "type"

rdf_first               :: ScopedName
rdf_first               = ScopedName namespaceRDF  "first"

rdf_rest                :: ScopedName
rdf_rest                = ScopedName namespaceRDF  "rest"

rdf_nil                 :: ScopedName
rdf_nil                 = ScopedName namespaceRDF  "nil"

rdf_XMLLiteral          :: ScopedName
rdf_XMLLiteral          = ScopedName namespaceRDF  "XMLLiteral"

rdfs_member             :: ScopedName
rdfs_member             = ScopedName namespaceRDFS "member"

rdfd_GeneralRestriction :: ScopedName
rdfd_GeneralRestriction = ScopedName namespaceRDFD "GeneralRestriction"

rdfd_onProperties       :: ScopedName
rdfd_onProperties       = ScopedName namespaceRDFD "onProperties"

rdfd_constraint         :: ScopedName
rdfd_constraint         = ScopedName namespaceRDFD "constraint"

rdfd_maxCardinality     :: ScopedName
rdfd_maxCardinality     = ScopedName namespaceRDFD "maxCardinality"

xsd_type                :: String -> ScopedName
xsd_type                = ScopedName namespaceXSD

xsd_string              :: ScopedName
xsd_string              = xsd_type "string"

xsd_boolean             :: ScopedName
xsd_boolean             = xsd_type "boolean"

xsd_decimal             :: ScopedName
xsd_decimal             = xsd_type "decimal"

xsd_integer             :: ScopedName
xsd_integer             = xsd_type "integer"

xsd_nonneg_integer      :: ScopedName
xsd_nonneg_integer      = xsd_type "nonNegativeInteger"

xsd_nonpos_integer      :: ScopedName
xsd_nonpos_integer      = xsd_type "nonPositiveInteger"

xsd_pos_integer         :: ScopedName
xsd_pos_integer         = xsd_type "positiveInteger"

xsd_neg_integer         :: ScopedName
xsd_neg_integer         = xsd_type "negativeInteger"

xsd_float               :: ScopedName
xsd_float               = xsd_type "float"

xsd_double              :: ScopedName
xsd_double              = xsd_type "double"

xsd_date, xsd_dateTime :: ScopedName
xsd_date = xsd_type "date"
xsd_dateTime = xsd_type "dateTime"

owl_sameAs              :: ScopedName
owl_sameAs              = ScopedName namespaceOWL  "sameAs"

log_implies             :: ScopedName
log_implies             = ScopedName namespaceLOG "implies"

default_base            :: ScopedName
default_base            = ScopedName namespaceDefault "base"

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
