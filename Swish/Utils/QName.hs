{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QName
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
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

module Swish.Utils.QName
    ( QName
    , newQName
    , qnameFromURI
    , getNamespace
    , getLocalName
    , getQNameURI
    , qnameFromFilePath
    )
    where

import System.Directory (canonicalizePath)

import Network.URI (URI(..), URIAuth(..)
                    , parseURIReference)

import Data.String (IsString(..))
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

------------------------------------------------------------
--  Qualified name
------------------------------------------------------------
--
--  cf. http://www.w3.org/TR/REC-xml-names/

{-| 

A qualified name, consisting of a namespace URI
and the local part of the identifier.

-}

{-
For now I have added in storing the actual URI
as well as the namespace component. This may or
may not be a good idea (space vs time saving).
-}

data QName = QName
             { qnURI :: URI       -- ^ URI
             , qnNsuri :: URI     -- ^ namespace 
             , qnLocal :: T.Text  -- ^ local component
             }

-- | This is not total since it will fail if the input string is not a valid URI.
instance IsString QName where
  fromString s =   
    maybe (error ("Unable to convert " ++ s ++ " into a QName"))
          qnameFromURI (parseURIReference s)

instance Eq QName where
    (==) = qnEq

-- ugly, use show instance
    
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
  
-- The format of show QName may well change to remove the <>
instance Show QName where
    show (QName u _ _) = "<" ++ show u ++ ">"

{-
Should this be clever and ensure that local doesn't
contain /, say?

We could also me more clever, and safer, when constructing
the overall uri.
-}
newQName :: URI -> T.Text -> QName
newQName ns local = 
  let l   = T.unpack local
      uristr = show ns ++ l
      uri = fromMaybe (error ("Unable to parse URI from: '" ++ show ns ++ "' + '" ++ l ++ "'")) (parseURIReference uristr)
  
  {- the following does not work since the semantics of relativeTo do not match the required
     behavior here.  It may well be better to do something like the following, writing a replacement for
     relativeTo, but leave that for a later date.
  
  let l   = T.unpack local
      luri = fromMaybe (error ("Unable to parse local name as a URI reference: '" ++ l ++ "'")) (parseRelativeReference l)
      uri = fromMaybe (error ("Unable to combine " ++ show ns ++ " with " ++ l)) $ luri `relativeTo` ns
  -}
      
  in QName uri ns local

{-

old behavior

 splitQname "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
 splitQname "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
 splitQname "http://example.org/aaa/"    = ("http://example.org/aaa/","")

Should "urn:foo:bar" have a local name of "" or "foo:bar"? For now go
with the first option.

-}

qnameFromURI :: URI -> QName
qnameFromURI uri =
  let uf = uriFragment uri
      up = uriPath uri
      q0 = QName uri uri ""
  in case uf of
    "#"    -> q0
    '#':xs -> QName uri (uri { uriFragment = "#" }) (T.pack xs)
    ""     -> case break (=='/') (reverse up) of
      ("",_) -> q0 -- path ends in / or is empty
      (_,"") -> q0 -- path contains no /
      (rlname,rpath) -> QName uri (uri {uriPath = reverse rpath}) (T.pack (reverse rlname))
      
    e -> error $ "Unexpected: uri=" ++ show uri ++ " has fragment='" ++ show e ++ "'" 

-- | Return the URI of the namespace stored in the QName.
-- This does not contain the local component.
--
getNamespace :: QName -> URI
getNamespace = qnNsuri

-- | Return the local component of the QName.
getLocalName :: QName -> T.Text
getLocalName = qnLocal

-- | Returns the full URI of the QName (ie the combination of the
-- namespace and local components).
getQNameURI :: QName -> URI
getQNameURI = qnURI

{-
Original used comparison of concatenated strings,
but that was very inefficient.  The longer version below
does the comparison without constructing new values but is
no longer valid with the namespace being stored as a URI,
so for now just compare the overall URIs and we can
optimize this at a later date if needed.
-}
qnEq :: QName -> QName -> Bool
qnEq (QName u1 _ _) (QName u2 _ _) = u1 == u2
{-
qnEq (QName _ n1 l1) (QName _ n2 l2) = qnEq1 n1 n2 l1 l2
  where
    qnEq1 (c1:ns1) (c2:ns2)  ln1 ln2   = c1==c2 && qnEq1 ns1 ns2 ln1 ln2
    qnEq1 []  ns2  ln1@(_:_) ln2       = qnEq1 ln1 ns2 []  ln2
    qnEq1 ns1 []   ln1       ln2@(_:_) = qnEq1 ns1 ln2 ln1 []
    qnEq1 []  []   []        []        = True
    qnEq1 _   _    _         _         = False
-}

-- Definitions here per XML namespaces, NCName production,
-- restricted to characters used in URIs.
-- cf. http://www.w3.org/TR/REC-xml-names/

{-
isNameStartChar :: Char -> Bool
isNameStartChar c = isAlpha c || c == '_'

isNameChar :: Char -> Bool
isNameChar      c = isAlphaNum c || c `elem` ".-_"
-}

{-|
Convert a filepath to a file: URI stored in a QName. If the
input file path is relative then the working directory is used
to convert it into an absolute path.

If the input represents a directory then it *must* end in 
the directory separator - so for Posix systems use 
@\"\/foo\/bar\/\"@ rather than 
@\"\/foo\/bar\"@.

This has not been tested on Windows.
-}

{-
NOTE: not sure what I say directories should end in the path
seperator since

ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text"
"/Users/dburke/haskell/swish-text"
ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text/"
"/Users/dburke/haskell/swish-text"

-}

-- since we build up the URI manually we could
-- create the QName directly, but leave that 
-- for now.

qnameFromFilePath :: FilePath -> IO QName
qnameFromFilePath = fmap qnameFromURI . filePathToURI
  
emptyAuth :: Maybe URIAuth
emptyAuth = Just $ URIAuth "" "" ""

filePathToURI :: FilePath -> IO URI
filePathToURI fname = do
  ipath <- canonicalizePath fname
  
  {-
  let paths = splitDirectories ipath
      txt = intercalate "/" $ case paths of
        "/":rs -> rs
        _      -> paths
  -}
  
  -- Is manually creating the URI sensible?
  -- return $ fromJust $ parseURI $ "file:///" ++ txt
  -- return $ URI "file:" emptyAuth txt "" ""
  return $ URI "file:" emptyAuth ipath "" ""

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
