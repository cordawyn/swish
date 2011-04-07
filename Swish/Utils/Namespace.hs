{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
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
--  Portability :  TypeSynonymInstances, MultiParamTypeClasses
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
    , nullNamespace
    , ScopedName(..)
    , getScopePrefix, getScopeURI
    , getQName, getScopedNameURI
    , matchName
    , makeScopedName, makeQNameScopedName, makeUriScopedName
    , nullScopedName
    )
where

import Swish.Utils.QName
    ( QName(..), getQNameURI )

import Swish.Utils.LookupMap
    ( LookupEntryClass(..) )

------------------------------------------------------------
--  Namespace, having a prefix and a URI
------------------------------------------------------------

-- |A NameSpace value consists of a prefix and a corresponding URI.
--  The prefix may be empty (@\"\"@), in which case it is assumed to be unknown.
--
-- NOTE: not clear whether @nsPrefix@ should be empty or set to @\"?\"@.
--
data Namespace = Namespace { nsPrefix :: String, nsURI :: String }

{-
getNamespacePrefix :: Namespace -> String
getNamespacePrefix = nsPrefix

getNamespaceURI    :: Namespace -> String
getNamespaceURI    = nsURI
-}

instance Eq Namespace where
    (==) = nsEq

instance Show Namespace where
    show (Namespace p u) =
        (if p == "?" then "" else p ++ ":") ++ "<" ++ u ++ ">"

instance LookupEntryClass Namespace String String where
    keyVal   (Namespace pre uri) = (pre,uri)
    newEntry (pre,uri)           = Namespace pre uri

nsEq :: Namespace -> Namespace -> Bool
nsEq (Namespace _ u1) (Namespace _ u2) = u1 == u2

makeNamespaceQName :: Namespace -> String -> QName
makeNamespaceQName ns = QName (nsURI ns)

nullNamespace :: Namespace
nullNamespace = Namespace "?" ""

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
data ScopedName = ScopedName { snScope :: Namespace, snLocal :: String }

getScopePrefix :: ScopedName -> String
getScopePrefix = nsPrefix . snScope

getScopeURI :: ScopedName -> String
getScopeURI = nsURI . snScope

instance Eq ScopedName where
    (==) = snEq

instance Ord ScopedName where
    (<=) = snLe

instance Show ScopedName where
    show (ScopedName n l) =
        if pre == "?" then "<"++uri++l++">" else pre++":"++l
        where
            pre = nsPrefix n
            uri = nsURI n

--  Scoped names are equal of ther corresponding QNames are equal
snEq :: ScopedName -> ScopedName -> Bool
snEq s1 s2 = getQName s1 == getQName s2

--  Scoped names are ordered by their QNames
snLe :: ScopedName -> ScopedName -> Bool
snLe s1 s2 = getQName s1 <= getQName s2

-- |Get QName corresponding to a scoped name
getQName :: ScopedName -> QName
getQName n = QName (getScopeURI n) (snLocal n)

-- |Get URI corresponding to a scoped name (using RDF conventions)
getScopedNameURI :: ScopedName -> String
getScopedNameURI = getQNameURI . getQName

-- |Test if supplied string matches the display form of a
--  scoped name.
matchName :: String -> ScopedName -> Bool
matchName str nam = str == show nam

-- |Construct a ScopedName from prefix, URI and local name
makeScopedName :: String -> String -> String -> ScopedName
makeScopedName pre nsuri =
    ScopedName (Namespace pre nsuri)

-- |Construct a ScopedName from a QName
makeQNameScopedName :: QName -> ScopedName
makeQNameScopedName (QName u l) = makeScopedName "?" u l

-- |Construct a ScopedName for a bare URI
makeUriScopedName :: String -> ScopedName
makeUriScopedName u = makeScopedName "?" u ""

-- |This should never appear as a valid name
nullScopedName :: ScopedName
nullScopedName = makeScopedName "?" "" ""

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
