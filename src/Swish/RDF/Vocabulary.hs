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
--  using the 'Namespace' and 'ScopedName' data types. Additional vocabularies
--  are available in the set of @Swish.RDF.Vocabulary.*@ modules, parts of
--  which are re-exported by this module
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary
    ( 
      -- * Namespaces
      
      namespaceRDFD
    , namespaceXsdType
    , namespaceMATH
    , namespaceLOG
    , namespaceDAML
    , namespaceDefault
    , namespaceSwish 

    -- ** RDF rules                                     
    -- | The namespaces refer to RDF rules and axioms.                                     
    , scopeRDF
    , scopeRDFS
    , scopeRDFD

    -- * Language tags
    --
    -- | Support for language tags that follow RFC 3066.
    -- 
    -- This replaces the use of @ScopedName@ and @langName@, @langTag@,
    -- and @isLang@ in versions prior to @0.7.0.0@.
    --
    , LanguageTag
    , toLangTag
    , fromLangTag
    
    -- * Miscellaneous routines
    , swishName
    , rdfdGeneralRestriction
    , rdfdOnProperties, rdfdConstraint, rdfdMaxCardinality
    , logImplies
    , defaultBase
      
    -- * Re-exported modules  
    , module Swish.RDF.Vocabulary.RDF
    , module Swish.RDF.Vocabulary.OWL
    , module Swish.RDF.Vocabulary.XSD  
    )
where

import Swish.RDF.Vocabulary.RDF
import Swish.RDF.Vocabulary.OWL
import Swish.RDF.Vocabulary.XSD

import Swish.Utils.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)

import Data.Char (isDigit)
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

-- | Maps @rdfd@ to @http:\/\/id.ninebynine.org\/2003\/rdfext\/rdfd#@.
namespaceRDFD :: Namespace
namespaceRDFD    = toNSU "rdfd"   namespaceRDFDURI

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

tU :: String -> URI
tU = fromMaybe (error "Internal error processing namespace URI") . parseURI

namespaceRDFDURI, 
  namespaceLOGURI,
  namespaceSwishURI, 
  namespaceDefaultURI :: URI
namespaceRDFDURI  = tU "http://id.ninebynine.org/2003/rdfext/rdfd#"
namespaceLOGURI   = tU "http://www.w3.org/2000/10/swap/log#"
namespaceSwishURI = tU "http://id.ninebynine.org/2003/Swish/"
namespaceDefaultURI = tU "http://id.ninebynine.org/default/"

-- | Convert a local name to a scoped name in the @swish@ namespace (`namespaceSwish`).
swishName :: T.Text -> ScopedName
swishName = makeNSScopedName namespaceSwish

-----------------------------------------------------------
--  Language tags
------------------------------------------------------------
--
--  Note:  simple language tag URIs may be abbreviated as lang:tag,
--  but if the tag contains a hyphen, this would not be valid QName
--  form in Notation3, even though it is a valid QName component.
--  Fortunately, they do not currently need to appear in Notation3 as
--  distinct labels (but future developments may change that).

-- | Represent the language tag for a literal string, following
-- RFC 3066 <http://www.ietf.org/rfc/rfc3066.txt>.
--
-- Use 'toLangTag' to create a tag and 'fromLangTag' to
-- convert back. The only guarantee is that
--
-- > (fromLangTag . toLangTag) lt == T.toLower lt
--
data LanguageTag = 
    LanguageTag 
    T.Text       -- ^ full value
    T.Text       -- ^ primary tag
    [T.Text]     -- ^ sub tags

instance Show LanguageTag where
    show = T.unpack . fromLangTag

-- | The equality test matches on the full definition, so
-- @en-GB@ does not match @en@.
instance Eq LanguageTag where
    LanguageTag f1 _ _ == LanguageTag f2 _ _ = f1 == f2

-- | Create a 'LanguageTag' element from the label.
-- 
-- Valid tags follow the ABNF from RCF 3066, which is
--
--    Language-Tag = Primary-subtag *( "-" Subtag )
--    Primary-subtag = 1*8ALPHA
--    Subtag = 1*8(ALPHA / DIGIT)
--
-- There are no checks that the primary or secondary sub tag
-- values are defined in any standard, such as ISO 639,
-- or obey any other syntactical restriction than given above.
-- 
toLangTag :: T.Text -> Maybe LanguageTag
toLangTag lbl = 
    let tag = T.toLower lbl
        toks = T.split (=='-') tag
    in if all (\s -> let l = T.length s in l > 0 && l < 9) toks
       then let primtag : subtags = toks
                isVChar c = c >= 'a' && c <= 'z'
            in if T.all isVChar primtag && all (T.all (\c -> isVChar c || isDigit c)) subtags
               then Just $ LanguageTag tag primtag subtags
               else Nothing
       else Nothing

-- | Convert a language tag back into text form.
fromLangTag :: LanguageTag -> T.Text
fromLangTag (LanguageTag f _ _) = f

-- TODO: use Language Range (section 2.5 of RFC 3066) to support
-- language comparison

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

toRDFD :: T.Text -> ScopedName
toRDFD = makeNSScopedName namespaceRDFD

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

-- | @log:implies@.
logImplies  :: ScopedName
logImplies  = makeNSScopedName namespaceLOG "implies"

-- | @default:base@.
defaultBase :: ScopedName
defaultBase = makeNSScopedName namespaceDefault "base"

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
