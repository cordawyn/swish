{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Namespace
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings
--
--  This module defines algebraic datatypes for namespaces and scoped names.
--
--  For these purposes, a namespace is a prefix and URI used to identify
--  a namespace (cf. XML namespaces), and a scoped name is a name that
--  is scoped by a specified namespace.
--
--------------------------------------------------------------------------------

module Swish.Utils.Namespace
    ( Namespace(..)
    , makeNamespaceQName
    -- , nullNamespace
    , ScopedName
    , getScopeNamespace, getScopeLocal
    , getScopePrefix, getScopeURI
    , getQName, getScopedNameURI
    , matchName
    , makeScopedName
    , makeQNameScopedName
    , makeURIScopedName
    , makeNSScopedName
    , nullScopedName
    , namespaceToBuilder
    )
    where

import Swish.Utils.QName (QName, newQName, getQNameURI, getNamespace, getLocalName)
import Swish.Utils.LookupMap (LookupEntryClass(..))

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)

import Network.URI (URI(..), parseURIReference, nullURI)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

------------------------------------------------------------
--  Namespace, having a prefix and a URI
------------------------------------------------------------

-- |A NameSpace value consists of an optional prefix and a corresponding URI.
--

data Namespace = Namespace
                 {
                   nsPrefix :: Maybe T.Text
                 , nsURI :: URI
                 }
                 
instance Eq Namespace where
    (==) = nsEq

instance Show Namespace where
    show (Namespace (Just p) u) = show p ++ ":<" ++ show u ++ ">"
    show (Namespace _ u)        = "<" ++ show u ++ ">"

instance LookupEntryClass Namespace (Maybe T.Text) URI where
    keyVal   (Namespace pre uri) = (pre,uri)
    newEntry (pre,uri)           = Namespace pre uri

nsEq :: Namespace -> Namespace -> Bool
nsEq (Namespace _ u1) (Namespace _ u2) = u1 == u2

makeNamespaceQName :: Namespace -> T.Text -> QName
makeNamespaceQName (Namespace _ uri) = newQName uri

{-
nullNamespace :: Namespace
nullNamespace = Namespace Nothing ""
-}

-- | Utility routine to create a \@prefix line (matching N3/Turtle)
--   grammar for this namespace.
--
namespaceToBuilder :: Namespace -> B.Builder
namespaceToBuilder (Namespace pre uri) =
  mconcat $ map B.fromText 
  [ "@prefix ", fromMaybe "" pre, ": <", T.pack (show uri), "> .\n"]

------------------------------------------------------------
--  ScopedName, made from a namespace and a local name
------------------------------------------------------------

-- |A full ScopedName value has a QName prefix, namespace URI
--  and a local part.  ScopedName values may omit the prefix
--  (see 'Namespace') or the local part.
--
--  Some applications may handle null namespace URIs as meaning
--  the local part is relative to some base URI.
--
data ScopedName = ScopedName 
                  { snQName :: QName  -- ^ the full URI as a QName (optimisation, may be removed)
                  , snScope :: Namespace
                  , snLocal :: T.Text 
                  }

getScopeLocal :: ScopedName -> T.Text
getScopeLocal = snLocal

getScopeNamespace :: ScopedName -> Namespace
getScopeNamespace = snScope

getScopePrefix :: ScopedName -> Maybe T.Text
getScopePrefix = nsPrefix . snScope

getScopeURI :: ScopedName -> URI
getScopeURI = nsURI . snScope

-- | This is not total since it will fail if the input string is not a valid URI.
instance IsString ScopedName where
  fromString s =
    maybe (error ("Unable to convert " ++ s ++ " into a ScopedName"))
          makeURIScopedName (parseURIReference s)
    
instance Eq ScopedName where
    (==) = snEq

instance Ord ScopedName where
    (<=) = snLe

instance Show ScopedName where
    show (ScopedName _ n l) = case nsPrefix n of
      Just pre -> T.unpack $ mconcat [pre, ":", l]
      _        -> "<" ++ show (nsURI n) ++ T.unpack l ++ ">"

--  Scoped names are equal if their corresponding QNames are equal
snEq :: ScopedName -> ScopedName -> Bool
snEq s1 s2 = getQName s1 == getQName s2

--  Scoped names are ordered by their QNames
snLe :: ScopedName -> ScopedName -> Bool
snLe s1 s2 = getQName s1 <= getQName s2

-- |Get QName corresponding to a scoped name
getQName :: ScopedName -> QName
-- getQName n = newQName (getScopeURI n) (snLocal n)
getQName = snQName

-- |Get URI corresponding to a scoped name (using RDF conventions)
getScopedNameURI :: ScopedName -> URI
getScopedNameURI = getQNameURI . getQName

-- for the moment leave this as String rather than Text

-- |Test if supplied string matches the display form of a
--  scoped name.
matchName :: String -> ScopedName -> Bool
matchName str nam = str == show nam

-- |Construct a ScopedName from prefix, URI and local name
makeScopedName :: Maybe T.Text -> URI -> T.Text -> ScopedName
makeScopedName pre nsuri local =
  ScopedName (newQName nsuri local) (Namespace pre nsuri) local

{-
TODO: should just pass URIs around.

At the moment support the use of URI references.  Unclear of semantics
to know whether this is sensible (probably is, but should look at).
-}

-- |Construct a ScopedName from a QName
makeQNameScopedName :: Maybe T.Text -> QName -> ScopedName
makeQNameScopedName pre qn = ScopedName qn (Namespace pre (getNamespace qn)) (getLocalName qn)
{-  
  let ns = getNamespace qn
      ln = getLocalName qn
  in makeScopedName Nothing ns ln
-}

-- | Construct a ScopedName for a bare URI (the label is set to \"\").
makeURIScopedName :: URI -> ScopedName
makeURIScopedName uri = makeScopedName Nothing uri ""

-- | Construct a ScopedName from a Namespace and local component
makeNSScopedName :: Namespace -> T.Text -> ScopedName
makeNSScopedName ns local = ScopedName (newQName (nsURI ns) local) ns local

-- | This should never appear as a valid name
nullScopedName :: ScopedName
nullScopedName = makeURIScopedName nullURI

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
