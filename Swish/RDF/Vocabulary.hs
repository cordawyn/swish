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
    , namespaceSwish 
    , swishName
    , namespaceLang,  langName, langTag, isLang
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    -- * RDF terms                                          
    --                                          
    -- | These terms are described in <http://www.w3.org/TR/rdf-syntax-grammar/>;                                          
    -- the version used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/>.
    --                                          
    -- Some terms are listed within the RDF Schema terms below since their definition                                            
    -- is given within the RDF Schema document.                                          
    --                                          
    , rdfRDF                                          
    , rdfDescription      
    , rdfID
    , rdfAbout
    , rdfParseType
    , rdfResource
    , rdfLi
    , rdfNodeID
    , rdfDatatype
    , rdf1, rdf2, rdfn
    -- * RDF Schema terms
    --
    -- | These are defined by <http://www.w3.org/TR/rdf-schema/>; the version
    -- used is \"W3C Recommendation 10 February 2004\", <http://www.w3.org/TR/2004/REC-rdf-schema-20040210/>.
                  
    -- ** Classes
    --
    -- | See the \"Classes\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , rdfsResource
    , rdfsClass
    , rdfsLiteral
    , rdfsDatatype
    , rdfXMLLiteral
    , rdfProperty
    
    -- ** Properties
    --                                 
    -- | See the \"Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_classes> for more information.
    , rdfsRange
    , rdfsDomain
    , rdfType
    , rdfsSubClassOf
    , rdfsSubPropertyOf
    , rdfsLabel
    , rdfsComment
    -- ** Containers
    --
    -- | See the \"Container Classes and Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_containervocab>.
    , rdfsContainer
    , rdfBag
    , rdfSeq                                 
    , rdfAlt  
    , rdfsContainerMembershipProperty
    , rdfsMember
    -- ** Collections
    --
    -- | See the \"Collections\" section at <http://www.w3.org/TR/rdf-schema/#ch_collectionvocab>.
    , rdfList    
    , rdfFirst
    , rdfRest 
    , rdfNil 
    -- ** Reification Vocabulary 
    --  
    -- | See the \"Reification Vocabulary\" section at <http://www.w3.org/TR/rdf-schema/#ch_reificationvocab>.
    , rdfStatement  
    , rdfSubject  
    , rdfPredicate  
    , rdfObject  
    -- ** Utility Properties 
    --  
    -- | See the \"Utility Properties\" section at <http://www.w3.org/TR/rdf-schema/#ch_utilvocab>.
    , rdfsSeeAlso
    , rdfsIsDefinedBy
    , rdfValue  
    -- * Miscellaneous     
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

import Swish.Utils.Namespace (Namespace, makeNamespace, ScopedName, getScopeLocal, getScopeNamespace, makeScopedName)

import Data.Monoid (mappend, mconcat)
import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

import qualified Data.Text as T

------------------------------------------------------------
--  Define some common namespace values
------------------------------------------------------------

toNS :: T.Text -> T.Text -> Namespace
toNS p utxt = 
  let ustr = T.unpack utxt
      uri = fromMaybe (error ("Unable to convert " ++ ustr ++ " to a URI")) $
            parseURI ustr
  in makeNamespace (Just p) uri

toNSU :: T.Text -> URI -> Namespace
toNSU p = makeNamespace (Just p)

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

namespaceRDF     = toNSU "rdf"    namespaceRDFURI
namespaceRDFS    = toNSU "rdfs"   namespaceRDFSURI
namespaceRDFD    = toNSU "rdfd"   namespaceRDFDURI
namespaceOWL     = toNSU "owl"    namespaceOWLURI
namespaceXSD     = toNSU "xsd"    namespaceXSDURI
namespaceMATH    = toNS "math"    "http://www.w3.org/2000/10/swap/math#"
namespaceLOG     = toNSU "log"    namespaceLOGURI
namespaceDAML    = toNS "daml"    "http://www.daml.org/2000/10/daml-ont#"
namespaceSwish   = toNSU "swish"  namespaceSwishURI
namespaceLang    = toNSU "lang"   namespaceLangURI
namespaceDefault = toNSU "default" namespaceDefaultURI

tU :: String -> URI
tU = fromMaybe (error "Internal error processing namespace URI") . parseURI

namespaceRDFURI, namespaceRDFSURI, namespaceRDFDURI, 
  namespaceXSDURI, namespaceOWLURI, namespaceLOGURI,
  namespaceSwishURI, 
  namespaceLangURI, namespaceDefaultURI :: URI
namespaceRDFURI   = tU "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
namespaceRDFSURI  = tU "http://www.w3.org/2000/01/rdf-schema#"
namespaceRDFDURI  = tU "http://id.ninebynine.org/2003/rdfext/rdfd#"
namespaceOWLURI   = tU "http://www.w3.org/2002/07/owl#"
namespaceXSDURI   = tU "http://www.w3.org/2001/XMLSchema#"
namespaceLOGURI   = tU "http://www.w3.org/2000/10/swap/log#"
namespaceSwishURI = tU "http://id.ninebynine.org/2003/Swish/"
namespaceLangURI  = tU "http://id.ninebynine.org/2003/Swish/Lang/" -- To be replaced by urn:ietf:params:lang?  
namespaceDefaultURI = tU "http://id.ninebynine.org/default/"

swishName :: T.Text -> ScopedName
-- swishName = ScopedName namespaceSwish
swishName = makeScopedName (Just "swish") namespaceSwishURI

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
-- langName = ScopedName namespaceLang . T.toLower
langName = makeScopedName (Just "lang") namespaceLangURI . T.toLower

langTag :: ScopedName -> T.Text
langTag = getScopeLocal

isLang :: ScopedName -> Bool
isLang sname = getScopeNamespace sname == namespaceLang

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
toRDF  = makeScopedName (Just "rdf")  namespaceRDFURI
toRDFS = makeScopedName (Just "rdfs") namespaceRDFSURI
toRDFD = makeScopedName (Just "rdfd") namespaceRDFDURI

-- | @rdf:RDF@
rdfRDF :: ScopedName
rdfRDF = toRDF "RDF"

-- | @rdf:Description@
rdfDescription :: ScopedName
rdfDescription = toRDF "Description"
  
-- | @rdf:datatype@
rdfDatatype   :: ScopedName
rdfDatatype   = toRDF "datatype"

-- | @rdf:resource@
rdfResource   :: ScopedName
rdfResource   = toRDF "resource"

-- | @rdf:about@
rdfAbout      :: ScopedName
rdfAbout      = toRDF "about"

-- | @rdf:ID@
rdfID         :: ScopedName
rdfID         = toRDF "ID"

-- | @rdf:parseType@
rdfParseType :: ScopedName
rdfParseType = toRDF "parseType"

-- | @rdf:li@
rdfLi :: ScopedName
rdfLi = toRDF "li"

-- | @rdf:nodeID@
rdfNodeID :: ScopedName
rdfNodeID = toRDF "nodeID"

-- | Create a @rdf:_n@ entity.
rdfn :: Int -> ScopedName
rdfn = toRDF . T.pack . ("_" ++) . show

-- | @rdf:_1@
rdf1 :: ScopedName
rdf1 = toRDF "_1"

-- | @rdf:_2@
rdf2 :: ScopedName
rdf2 = toRDF "_2"

-- | @rdf:first@ from <http://www.w3.org/TR/rdf-schema/#ch_first>.
rdfFirst      :: ScopedName
rdfFirst      = toRDF "first"

-- | @rdf:rest@ from <http://www.w3.org/TR/rdf-schema/#ch_rest>.
rdfRest       :: ScopedName
rdfRest       = toRDF "rest"

-- | @rdf:nil@ from <http://www.w3.org/TR/rdf-schema/#ch_nil>.
rdfNil        :: ScopedName
rdfNil        = toRDF "nil"

-- | @rdf:type@ from <http://www.w3.org/TR/rdf-schema/#ch_type>.
rdfType       :: ScopedName
rdfType       = toRDF "type"

-- | @rdf:Property@ from <http://www.w3.org/TR/rdf-schema/#ch_property>.
rdfProperty   :: ScopedName
rdfProperty   = toRDF "Property"

-- | @rdf:XMLLiteral@ from <http://www.w3.org/TR/rdf-schema/#ch_xmlliteral>.
rdfXMLLiteral :: ScopedName
rdfXMLLiteral = toRDF "XMLLiteral"

-- | @rdfs:Resource@ from <http://www.w3.org/TR/rdf-schema/#ch_resource>.
rdfsResource :: ScopedName
rdfsResource = toRDFS "Resource"

-- | @rdfs:Class@ from <http://www.w3.org/TR/rdf-schema/#ch_class>.
rdfsClass :: ScopedName
rdfsClass = toRDFS "Class"

-- | @rdfs:Literal@ from <http://www.w3.org/TR/rdf-schema/#ch_literal>.
rdfsLiteral :: ScopedName
rdfsLiteral = toRDFS "Literal"

-- | @rdfs:Datatype@ from <http://www.w3.org/TR/rdf-schema/#ch_datatype>.
rdfsDatatype :: ScopedName
rdfsDatatype = toRDFS "Datatype"

-- | @rdfs:label@ from <http://www.w3.org/TR/rdf-schema/#ch_label>.
rdfsLabel :: ScopedName
rdfsLabel = toRDFS "label"

-- | @rdfs:comment@ from <http://www.w3.org/TR/rdf-schema/#ch_comment>.
rdfsComment :: ScopedName
rdfsComment = toRDFS "comment"

-- | @rdfs:range@ from <http://www.w3.org/TR/rdf-schema/#ch_range>.
rdfsRange :: ScopedName
rdfsRange = toRDFS "range"

-- | @rdfs:domain@ from <http://www.w3.org/TR/rdf-schema/#ch_domain>.
rdfsDomain :: ScopedName
rdfsDomain = toRDFS "domain"

-- | @rdfs:subClassOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subclassof>.
rdfsSubClassOf :: ScopedName
rdfsSubClassOf = toRDFS "subClassOf"

-- | @rdfs:subPropertyOf@ from <http://www.w3.org/TR/rdf-schema/#ch_subpropertyof>.
rdfsSubPropertyOf :: ScopedName
rdfsSubPropertyOf = toRDFS "subPropertyOf"

-- | @rdfs:Container@ from <http://www.w3.org/TR/rdf-schema/#ch_container>.
rdfsContainer :: ScopedName
rdfsContainer = toRDFS "Container"

-- | @rdf:Bag@ from <http://www.w3.org/TR/rdf-schema/#ch_bag>.
rdfBag :: ScopedName
rdfBag = toRDF "Bag"

-- | @rdf:Seq@ from <http://www.w3.org/TR/rdf-schema/#ch_seq>.
rdfSeq :: ScopedName
rdfSeq = toRDF "Seq"

-- | @rdf:Alt@ from <http://www.w3.org/TR/rdf-schema/#ch_alt>.
rdfAlt :: ScopedName
rdfAlt = toRDF "Alt"

-- | @rdfs:ContainerMembershipProperty@ from <http://www.w3.org/TR/rdf-schema/#ch_containermembershipproperty>.
rdfsContainerMembershipProperty :: ScopedName
rdfsContainerMembershipProperty = toRDFS "ContainerMembershipProperty"

-- | @rdfs:member@ from <http://www.w3.org/TR/rdf-schema/#ch_member>.
rdfsMember :: ScopedName
rdfsMember    = toRDFS "member"

-- | @rdf:List@ from <http://www.w3.org/TR/rdf-schema/#ch_list>.
rdfList :: ScopedName
rdfList = toRDF "List"

-- | @rdf:Statement@ from <http://www.w3.org/TR/rdf-schema/#ch_statement>.
rdfStatement :: ScopedName
rdfStatement = toRDF "Statement"

-- | @rdf:subject@ from <http://www.w3.org/TR/rdf-schema/#ch_subject>.
rdfSubject :: ScopedName
rdfSubject = toRDF "subject"

-- | @rdf:predicate@ from <http://www.w3.org/TR/rdf-schema/#ch_predicate>.
rdfPredicate :: ScopedName
rdfPredicate = toRDF "subject"

-- | @rdf:object@ from <http://www.w3.org/TR/rdf-schema/#ch_object>.
rdfObject :: ScopedName
rdfObject = toRDF "object"

-- | @rdfs:seeAlso@ from <http://www.w3.org/TR/rdf-schema/#ch_seealso>.
rdfsSeeAlso :: ScopedName
rdfsSeeAlso = toRDFS "seeAlso"

-- | @rdfs:isDefinedBy@ from <http://www.w3.org/TR/rdf-schema/#ch_isdefinedby>.
rdfsIsDefinedBy :: ScopedName
rdfsIsDefinedBy = toRDFS "isDefinedBy"

-- | @rdf:value@ from <http://www.w3.org/TR/rdf-schema/#ch_value>.
rdfValue :: ScopedName
rdfValue = toRDF "value"

rdfdGeneralRestriction :: ScopedName
rdfdOnProperties       :: ScopedName
rdfdConstraint         :: ScopedName
rdfdMaxCardinality     :: ScopedName

rdfdGeneralRestriction = toRDFD "GeneralRestriction"
rdfdOnProperties       = toRDFD "onProperties"
rdfdConstraint         = toRDFD "constraint"
rdfdMaxCardinality     = toRDFD "maxCardinality"

xsdType             :: T.Text -> ScopedName
xsdType             = makeScopedName (Just "xsd") namespaceXSDURI

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
owlSameAs   = makeScopedName (Just "owl") namespaceOWLURI "sameAs"

logImplies  :: ScopedName
logImplies  = makeScopedName (Just "log") namespaceLOGURI "implies"

defaultBase :: ScopedName
defaultBase = makeScopedName (Just "default") namespaceDefaultURI "base"

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
