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
    ( 
      -- * Namespaces
      
      namespaceRDF
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
    , namespaceLang
    -- ** RDF rules                                     
    -- | The namespaces refer to RDF rules and axioms.                                     
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    
    -- * Miscellaneous routines
    , langName, langTag, isLang
    , swishName
      
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
      
    -- * XSD data types
    --  
    -- | See the XSD Schema Part 2 documentation at <http://www.w3.org/TR/xmlschema-2/>;
    -- the version used is \"W3C Recommendation 28 October 2004\",  
    -- <http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/>.  
    , xsdType 
    
    -- ** Primitive datatypes  
    --  
    -- | See the section \"Primitive datatypes\" at  
    -- <http://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes>.  
    , xsdString
    , xsdBoolean
    , xsdDecimal
    , xsdFloat
    , xsdDouble
    , xsdDateTime
    , xsdTime
    , xsdDate
    , xsdAnyURI  
      
    -- ** Derived datatypes  
    --  
    -- | See the section \"Derived datatypes\" at  
    -- <http://www.w3.org/TR/xmlschema-2/#built-in-derived>.  
    , xsdInteger
    , xsdNonPosInteger
    , xsdNegInteger
    , xsdLong
    , xsdInt
    , xsdShort
    , xsdByte
    , xsdNonNegInteger
    , xsdUnsignedLong
    , xsdUnsignedInt
    , xsdUnsignedShort
    , xsdUnsignedByte
    , xsdPosInteger

    -- * OWL items
    
    , owlSameAs

    -- * Miscellaneous     
    , rdfdGeneralRestriction
    , rdfdOnProperties, rdfdConstraint, rdfdMaxCardinality
    , logImplies
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

-- | Create a namespace for the datatype family schema used by Swish.
namespaceXsdType ::
  T.Text  -- ^ lbl
  -> Namespace 
  -- ^ Namespace has prefix @xsd_lbl@ and
  -- URI of @http:\/\/id.ninebynine.org\/2003\/XMLSchema\/lbl#@.
namespaceXsdType dtn = toNS ("xsd_" `mappend` dtn)
                       (mconcat ["http://id.ninebynine.org/2003/XMLSchema/", dtn, "#"])

-- | Maps @rdf@ to <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
namespaceRDF :: Namespace
namespaceRDF     = toNSU "rdf"    namespaceRDFURI

-- | Maps @rdfs@ to <http://www.w3.org/2000/01/rdf-schema#>.
namespaceRDFS :: Namespace
namespaceRDFS    = toNSU "rdfs"   namespaceRDFSURI

-- | Maps @rdfd@ to @http:\/\/id.ninebynine.org\/2003\/rdfext\/rdfd#@.
namespaceRDFD :: Namespace
namespaceRDFD    = toNSU "rdfd"   namespaceRDFDURI

-- | Maps @owl@ to <http://www.w3.org/2002/07/owl#>.
namespaceOWL :: Namespace
namespaceOWL     = toNSU "owl"    namespaceOWLURI

-- | Maps @xsd@ to <http://www.w3.org/2001/XMLSchema#>.
namespaceXSD :: Namespace
namespaceXSD     = toNSU "xsd"    namespaceXSDURI

-- | Maps @math@ to <http://www.w3.org/2000/10/swap/math#>.
namespaceMATH :: Namespace
namespaceMATH    = toNS "math"    "http://www.w3.org/2000/10/swap/math#"

-- | Maps @log@ to <http://www.w3.org/2000/10/swap/log#>.
namespaceLOG :: Namespace
namespaceLOG     = toNSU "log"    namespaceLOGURI

-- | Maps @daml@ to <http://www.daml.org/2000/10/daml-ont#>.
namespaceDAML :: Namespace
namespaceDAML    = toNS "daml"    "http://www.daml.org/2000/10/daml-ont#"

-- | Maps @swish@ to @http:\/\/id.ninebynine.org\/2003\/Swish\/@.
namespaceSwish :: Namespace
namespaceSwish   = toNSU "swish"  namespaceSwishURI

-- | Maps @default@ to @http:\/\/id.ninebynine.org\/default\/@.
namespaceDefault :: Namespace
namespaceDefault = toNSU "default" namespaceDefaultURI

-- | Maps @lang@ to @http:\/\/id.ninebynine.org\/2003\/Swish\/Lang\/@.
namespaceLang :: Namespace
namespaceLang    = toNSU "lang"   namespaceLangURI


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

-- | Convert a local name to a scoped name in the @swish@ namespace (`namespaceSwish`).
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

-- | Convert the label to a scoped name in the @lang@ namespace (`namespaceLang`).
langName :: 
  T.Text  -- ^ The lower-case version of this label is used.
  -> ScopedName
-- langName = ScopedName namespaceLang . T.toLower
langName = makeScopedName (Just "lang") namespaceLangURI . T.toLower

-- | Get the name of the language tag (note that the result is
-- only guaranteed to be semantically valid if 'isLang' returns @True@
-- but that there is no enforcement of this requirement).
langTag :: ScopedName -> T.Text
langTag = getScopeLocal

-- | Is the scoped name in the `namespaceLang` namespace?
isLang :: ScopedName -> Bool
isLang sname = getScopeNamespace sname == namespaceLang

------------------------------------------------------------
--  Define namespaces for RDF rules, axioms, etc
------------------------------------------------------------

-- | Maps @rs_rdf@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdf#@.
scopeRDF :: Namespace
scopeRDF  = toNS "rs_rdf"   "http://id.ninebynine.org/2003/Ruleset/rdf#"

-- | Maps @rs_rdfs@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdfs#@.
scopeRDFS :: Namespace
scopeRDFS = toNS "rs_rdfs"  "http://id.ninebynine.org/2003/Ruleset/rdfs#"

-- | Maps @rs_rdfd@ to @http:\/\/id.ninebynine.org\/2003\/Ruleset\/rdfd#@.
scopeRDFD :: Namespace
scopeRDFD = toNS "rs_rdfd"  "http://id.ninebynine.org/2003/Ruleset/rdfd#"

------------------------------------------------------------
--  Define some common vocabulary terms
------------------------------------------------------------

toRDF, toRDFS, toRDFD :: T.Text -> ScopedName
toRDF  = makeScopedName (Just "rdf")  namespaceRDFURI
toRDFS = makeScopedName (Just "rdfs") namespaceRDFSURI
toRDFD = makeScopedName (Just "rdfd") namespaceRDFDURI

-- | @rdf:RDF@.
rdfRDF :: ScopedName
rdfRDF = toRDF "RDF"

-- | @rdf:Description@.
rdfDescription :: ScopedName
rdfDescription = toRDF "Description"
  
-- | @rdf:datatype@.
rdfDatatype   :: ScopedName
rdfDatatype   = toRDF "datatype"

-- | @rdf:resource@.
rdfResource   :: ScopedName
rdfResource   = toRDF "resource"

-- | @rdf:about@.
rdfAbout      :: ScopedName
rdfAbout      = toRDF "about"

-- | @rdf:ID@.
rdfID         :: ScopedName
rdfID         = toRDF "ID"

-- | @rdf:parseType@.
rdfParseType :: ScopedName
rdfParseType = toRDF "parseType"

-- | @rdf:li@.
rdfLi :: ScopedName
rdfLi = toRDF "li"

-- | @rdf:nodeID@.
rdfNodeID :: ScopedName
rdfNodeID = toRDF "nodeID"

-- | Create a @rdf:_n@ entity.
rdfn :: Int -> ScopedName
rdfn = toRDF . T.pack . ("_" ++) . show

-- | @rdf:_1@.
rdf1 :: ScopedName
rdf1 = toRDF "_1"

-- | @rdf:_2@.
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

-- | @rdfd:GeneralRestriction@.
rdfdGeneralRestriction :: ScopedName
rdfdGeneralRestriction = toRDFD "GeneralRestriction"

-- | @rdfd:onProperties@.
rdfdOnProperties :: ScopedName
rdfdOnProperties = toRDFD "onProperties"

-- | @rdfd:constraint@.
rdfdConstraint :: ScopedName
rdfdConstraint = toRDFD "constraint"

-- | @rdfd:maxCardinality@.
rdfdMaxCardinality :: ScopedName
rdfdMaxCardinality = toRDFD "maxCardinality"

-- | Create a scoped name for an XSD datatype with the given name.
xsdType :: T.Text -> ScopedName
xsdType = makeScopedName (Just "xsd") namespaceXSDURI

-- | @xsd:string@ from <http://www.w3.org/TR/xmlschema-2/#string>.
xsdString           :: ScopedName
xsdString           = xsdType "string"

-- | @xsd:boolean@ from <http://www.w3.org/TR/xmlschema-2/#boolean>.
xsdBoolean          :: ScopedName
xsdBoolean          = xsdType "boolean"

-- | @xsd:decimal@ from <http://www.w3.org/TR/xmlschema-2/#decimal>.
xsdDecimal          :: ScopedName
xsdDecimal          = xsdType "decimal"

-- | @xsd:integer@ from <http://www.w3.org/TR/xmlschema-2/#integer>.
xsdInteger          :: ScopedName
xsdInteger          = xsdType "integer"

-- | @xsd:nonNegativeInteger@ from <http://www.w3.org/TR/xmlschema-2/#nonNegativeInteger>.
xsdNonNegInteger   :: ScopedName
xsdNonNegInteger   = xsdType "nonNegativeInteger"

-- | @xsd:nonPositiveInteger@ from <http://www.w3.org/TR/xmlschema-2/#nonPositiveInteger>.
xsdNonPosInteger   :: ScopedName
xsdNonPosInteger   = xsdType "nonPositiveInteger"

-- | @xsd:positiveInteger@ from <http://www.w3.org/TR/xmlschema-2/#positiveInteger>.
xsdPosInteger      :: ScopedName
xsdPosInteger      = xsdType "positiveInteger"

-- | @xsd:negativeInteger@ from <http://www.w3.org/TR/xmlschema-2/#negativeInteger>.
xsdNegInteger      :: ScopedName
xsdNegInteger      = xsdType "negativeInteger"

-- | @xsd:float@ from <http://www.w3.org/TR/xmlschema-2/#float>.
xsdFloat            :: ScopedName
xsdFloat            = xsdType "float"

-- | @xsd:double@ from <http://www.w3.org/TR/xmlschema-2/#double>.
xsdDouble           :: ScopedName
xsdDouble           = xsdType "double"

-- | @xsd:long@ from <http://www.w3.org/TR/xmlschema-2/#long>.
xsdLong :: ScopedName
xsdLong = xsdType "long"

-- | @xsd:int@ from <http://www.w3.org/TR/xmlschema-2/#int>.
xsdInt :: ScopedName
xsdInt = xsdType "int"

-- | @xsd:short@ from <http://www.w3.org/TR/xmlschema-2/#short>.
xsdShort :: ScopedName
xsdShort = xsdType "short"

-- | @xsd:byte@ from <http://www.w3.org/TR/xmlschema-2/#byte>.
xsdByte :: ScopedName
xsdByte = xsdType "byte"

-- | @xsd:unsignedLong@ from <http://www.w3.org/TR/xmlschema-2/#unsignedLong>.
xsdUnsignedLong :: ScopedName
xsdUnsignedLong = xsdType "unsignedLong"

-- | @xsd:unsignedInt@ from <http://www.w3.org/TR/xmlschema-2/#unsignedInt>.
xsdUnsignedInt :: ScopedName
xsdUnsignedInt = xsdType "unsignedInt"

-- | @xsd:unsignedShort@ from <http://www.w3.org/TR/xmlschema-2/#unsignedShort>.
xsdUnsignedShort :: ScopedName
xsdUnsignedShort = xsdType "unsignedShort"

-- | @xsd:unsignedByte@ from <http://www.w3.org/TR/xmlschema-2/#unsignedByte>.
xsdUnsignedByte :: ScopedName
xsdUnsignedByte = xsdType "unsignedByte"

-- | @xsd:date@ from <http://www.w3.org/TR/xmlschema-2/#date>.
xsdDate :: ScopedName
xsdDate = xsdType "date"

-- | @xsd:dateTime@ from <http://www.w3.org/TR/xmlschema-2/#dateTime>.
xsdDateTime :: ScopedName
xsdDateTime = xsdType "dateTime"

-- | @xsd:time@ from <http://www.w3.org/TR/xmlschema-2/#time>.
xsdTime :: ScopedName
xsdTime = xsdType "time"

-- | @xsd:anyURI@ from <http://www.w3.org/TR/xmlschema-2/#anyURI>.
xsdAnyURI :: ScopedName
xsdAnyURI = xsdType "anyURI"

-- | @owl:sameAs@.
owlSameAs   :: ScopedName
owlSameAs   = makeScopedName (Just "owl") namespaceOWLURI "sameAs"

-- | @log:implies@.
logImplies  :: ScopedName
logImplies  = makeScopedName (Just "log") namespaceLOGURI "implies"

-- | @default:base@.
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
