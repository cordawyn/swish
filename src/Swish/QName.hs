{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QName
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines an algebraic datatype for qualified names (QNames).
--
--------------------------------------------------------------------------------

-- At present we support using URI references rather than forcing an absolute
-- URI. This is partly to support the existing tests (too lazy to resolve whether
-- the tests really should be using relative URIs in this case).

module Swish.QName
    ( QName
    , LName
    , emptyLName
    , newLName
    , getLName
    , newQName
    , qnameFromURI
    , getNamespace
    , getLocalName
    , getQNameURI
    , qnameFromFilePath
    )
    where

import Control.Monad (liftM)

import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Data.Interned (intern, unintern)
import Data.Interned.URI (InternedURI)

import Network.URI (URI(..), URIAuth(..), parseURIReference)

import System.Directory (canonicalizePath)
import System.FilePath (splitFileName)

import qualified Data.Text as T

------------------------------------------------------------
--  Qualified name
------------------------------------------------------------
--
--  These are RDF QNames rather than XML ones (as much as
--  RDF can claim to have them).
--


{-| A local name, which can be empty.

At present, the local name can not 
contain spaces or the \'#\', \':\', or \'/\' characters. This restriction is
experimental.
-}
newtype LName = LName T.Text
    deriving (Eq, Ord)

instance Show LName where
    show (LName t) = show t

-- | This is not total since attempting to convert a string
--   containing invalid characters will cause an error.
instance IsString LName where
    fromString s = 
        fromMaybe (error ("Invalid local name: " ++ s)) $
                  newLName (T.pack s)

-- | The empty local name.
emptyLName :: LName
emptyLName = LName ""

-- | Create a local name.
newLName :: T.Text -> Maybe LName
newLName l = if T.any (`elem` " #:/") l then Nothing else Just (LName l)

-- | Extract the local name.
getLName :: LName -> T.Text
getLName (LName l) = l

{-| 

A qualified name, consisting of a namespace URI
and the local part of the identifier, which can be empty.
The serialisation of a QName is formed by concatanating the
two components.

> Prelude> :set prompt "swish> "
> swish> :set -XOverloadedStrings
> swish> :m + Swish.QName
> swish> let qn1 = "http://example.com/" :: QName
> swish> let qn2 = "http://example.com/bob" :: QName
> swish> let qn3 = "http://example.com/bob/fred" :: QName
> swish> let qn4 = "http://example.com/bob/fred#x" :: QName
> swish> map getLocalName [qn1, qn2, qn3, qn4]
> ["","bob","fred","x"]
> swish> getNamespace qn1
> http://example.com
> swish> getNamespace qn2
> http://example.com
> swish> getNamespace qn3
> http://example.com/bob/
> swish> getNamespace qn4
> http://example.com/bob/fred#

-}

{-
For now I have added in storing the actual URI
as well as the namespace component. This may or
may not be a good idea (space vs time saving).
-}

data QName = QName !InternedURI URI LName

-- | This is not total since it will fail if the input string is not a valid URI.
instance IsString QName where
  fromString s = 
      fromMaybe (error ("QName conversion given an invalid URI: " ++ s))
      (parseURIReference s >>= qnameFromURI)

-- | Equality is determined by a case sensitive comparison of the               
-- URI.
instance Eq QName where
  -- see qnEq comments below
  (QName u1 _ _) == (QName u2 _ _) = u1 == u2

-- ugly, use show instance OR switch to the ordering of InternedURI

-- | At present the ordering is based on a comparison of the @Show@
-- instance of the URI.
instance Ord QName where
  {-
    (QName u1 l1) <= (QName u2 l2) =
        if up1 /= up2 then up1 <= up2 else (ur1++l1) <= (ur2++l2)
        where
            n   = min (length u1) (length u2)
            (up1,ur1) = splitAt n u1
            (up2,ur2) = splitAt n u2
  -}
  
  -- TODO: which is faster?
  -- Now we have changed to InternedURI, we could use the
  -- Ord instance of it, but it is unclear to me what the
  -- ordering means in that case, and whether the semantics
  -- matter here?
  (QName u1 _ _) <= (QName u2 _ _) = show u1 <= show u2
  
  {-
  (QName _ uri1 l1) <= (QName _ uri2 l2) =
    if up1 /= up2 then up1 <= up2 else (ur1 ++ T.unpack l1) <= (ur2 ++ T.unpack l2)
      where
        u1 = show uri1
        u2 = show uri2
        
        n   = min (length u1) (length u2)
        (up1,ur1) = splitAt n u1
        (up2,ur2) = splitAt n u2
  -}
  
-- | The format used to display the URI is @\<uri\>@.
instance Show QName where
    show (QName u _ _) = "<" ++ show u ++ ">"

{-
The assumption in QName is that the validation done in creating
the local name is sufficient to ensure that the combined 
URI is syntactically valid. Is this in fact true?
-}

-- | Create a new qualified name with an explicit local component.
--
newQName ::
    URI            -- ^ Namespace
    -> LName       -- ^ Local component
    -> QName
newQName ns l@(LName local) = 
  -- Until profiling shows that this is a time/space issue, we use
  -- the following code rather than trying to deconstruct the URI
  -- directly
  let lstr   = T.unpack local
      uristr = show ns ++ lstr
  in case parseURIReference uristr of
       Just uri -> QName (intern uri) ns l
       _ -> error $ "Unable to combine " ++ show ns ++ " with " ++ lstr
  
{-

old behavior

 splitQname "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
 splitQname "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
 splitQname "http://example.org/aaa/"    = ("http://example.org/aaa/","")

Should "urn:foo:bar" have a local name of "" or "foo:bar"? For now go
with the first option.

-}

-- | Create a new qualified name.
qnameFromURI :: 
    URI      -- ^ The URI will be deconstructed to find if it contains a local component.
    -> Maybe QName -- ^ The failure case may be removed.
qnameFromURI uri =
  let uf = uriFragment uri
      up = uriPath uri
      q0 = Just $ QName iuri uri emptyLName
      start = QName iuri
      iuri = intern uri
  in case uf of
       "#"    -> q0
       '#':xs -> start (uri {uriFragment = "#"}) `liftM` newLName (T.pack xs)
       ""     -> case break (=='/') (reverse up) of
                   ("",_) -> q0 -- path ends in / or is empty
                   (_,"") -> q0 -- path contains no /
                   (rlname,rpath) -> 
                       start (uri {uriPath = reverse rpath}) `liftM` 
                       newLName (T.pack (reverse rlname))

       -- e -> error $ "Unexpected: uri=" ++ show uri ++ " has fragment='" ++ show e ++ "'" 
       _ -> Nothing

-- | Return the URI of the namespace stored in the QName.
-- This does not contain the local component.
--
getNamespace :: QName -> URI
getNamespace (QName _ ns _) = ns

-- | Return the local component of the QName.
getLocalName :: QName -> LName
getLocalName (QName _ _ l) = l

-- | Returns the full URI of the QName (ie the combination of the
-- namespace and local components).
getQNameURI :: QName -> URI
getQNameURI (QName u _ _) = unintern u

{-
Original used comparison of concatenated strings,
but that was very inefficient.  The longer version below
does the comparison without constructing new values but is
no longer valid with the namespace being stored as a URI,
so for now just compare the overall URIs and we can
optimize this at a later date if needed.
qnEq :: QName -> QName -> Bool
qnEq (QName u1 _ _) (QName u2 _ _) = u1 == u2

qnEq (QName _ n1 l1) (QName _ n2 l2) = qnEq1 n1 n2 l1 l2
  where
    qnEq1 (c1:ns1) (c2:ns2)  ln1 ln2   = c1==c2 && qnEq1 ns1 ns2 ln1 ln2
    qnEq1 []  ns2  ln1@(_:_) ln2       = qnEq1 ln1 ns2 []  ln2
    qnEq1 ns1 []   ln1       ln2@(_:_) = qnEq1 ns1 ln2 ln1 []
    qnEq1 []  []   []        []        = True
    qnEq1 _   _    _         _         = False
-}

{-|
Convert a filepath to a file: URI stored in a QName. If the
input file path is relative then the current working directory is used
to convert it into an absolute path.

If the input represents a directory then it *must* end in 
the directory separator - so for Posix systems use 
@\"\/foo\/bar\/\"@ rather than 
@\"\/foo\/bar\"@.

This has not been tested on Windows.
-}

{-
NOTE: not sure why I say directories should end in the path
seperator since

ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text"
"/Users/dburke/haskell/swish-text"
ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text/"
"/Users/dburke/haskell/swish-text"

-}

qnameFromFilePath :: FilePath -> IO QName
qnameFromFilePath fname = do
  ipath <- canonicalizePath fname
  let (dname, lname) = splitFileName ipath
      nsuri = URI "file:" emptyAuth dname "" ""
      uri = URI "file:" emptyAuth ipath "" ""
  case lname of
    "" -> return $ QName (intern nsuri) nsuri emptyLName
    _  -> return $ QName (intern uri) nsuri (LName (T.pack lname))

emptyAuth :: Maybe URIAuth
emptyAuth = Just $ URIAuth "" "" ""

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
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
