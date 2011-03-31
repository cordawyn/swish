--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QName
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines an algebraic datatype for qualified names (QNames).
--
--------------------------------------------------------------------------------

module Swish.HaskellUtils.QName
    ( QName(..) -- , maybeQnEq
    , newQName, qnameFromPair, qnameFromURI
    , getNamespace, getLocalName, getQNameURI
    , splitURI
    , qnameFromFilePath
    )
where

import Data.Char (isAlpha, isAlphaNum)

import System.Directory (canonicalizePath)
import System.FilePath (splitDirectories)
import Data.List (intercalate)

------------------------------------------------------------
--  Qualified name
------------------------------------------------------------
--
--  cf. http://www.w3.org/TR/REC-xml-names/

data QName = QName { qnNsuri, qnLocal :: String }

newQName :: String -> String -> QName
newQName = QName

qnameFromPair :: (String,String) -> QName
qnameFromPair (ns,ln) = QName ns ln

qnameFromURI :: String -> QName
qnameFromURI = qnameFromPair . splitURI

getNamespace :: QName -> String
getNamespace = qnNsuri

getLocalName :: QName -> String
getLocalName = qnLocal

getQNameURI :: QName -> String
getQNameURI (QName ns ln) = ns++ln

instance Eq QName where
    (==) = qnEq

instance Ord QName where
    (QName u1 l1) <= (QName u2 l2) =
        if up1 /= up2 then up1 <= up2 else (ur1++l1) <= (ur2++l2)
        where
            n   = min (length u1) (length u2)
            (up1,ur1) = splitAt n u1
            (up2,ur2) = splitAt n u2

instance Show QName where
    show (QName ns ln) = "<" ++ ns ++ ln ++ ">"

--  Original used comparison of concatenated strings,
--  but that was very inefficient.  This version does the
--  comparison without constructing new values
qnEq :: QName -> QName -> Bool
qnEq (QName n1 l1) (QName n2 l2) = qnEq1 n1 n2 l1 l2
  where
    qnEq1 (c1:ns1) (c2:ns2)  ln1 ln2   = c1==c2 && qnEq1 ns1 ns2 ln1 ln2
    qnEq1 []  ns2  ln1@(_:_) ln2       = qnEq1 ln1 ns2 []  ln2
    qnEq1 ns1 []   ln1       ln2@(_:_) = qnEq1 ns1 ln2 ln1 []
    qnEq1 []  []   []        []        = True
    qnEq1 _   _    _         _         = False

{-
--  Define equality of (Maybe QName)
maybeQnEq :: (Maybe QName) -> (Maybe QName) -> Bool
maybeQnEq Nothing   Nothing   = True
maybeQnEq (Just q1) (Just q2) = q1 == q2
maybeQnEq _         _         = False
-}

-- Separate URI string into namespace URI and local name
splitURI :: String -> ( String, String )
  -- splitQname "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
  -- splitQname "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
  -- splitQname "http://example.org/aaa/"    = ("http://example.org/aaa/","")
splitURI qn = splitAt (scanURI qn (-1) 0) qn

-- helper function for splitQName
-- Takes 3 arguments:
--   QName to scan
--   index of last name-start char, or (-1)
--   number of characters scanned so far
-- Returns index of start of name, or length of list
--
scanURI :: String -> Int -> Int -> Int
scanURI (nextch:more) (-1) nc
    | isNameStartChar nextch  = scanURI more nc   (nc+1)
    | otherwise               = scanURI more (-1) (nc+1)
scanURI (nextch:more) ns nc
    | not (isNameChar nextch) = scanURI more (-1) (nc+1)
    | otherwise               = scanURI more ns   (nc+1)
scanURI "" (-1) nc = nc
scanURI "" ns   _  = ns



-- Definitions here per XML namespaces, NCName production,
-- restricted to characters used in URIs.
-- cf. http://www.w3.org/TR/REC-xml-names/

isNameStartChar :: Char -> Bool
isNameStartChar c = isAlpha c || c == '_'

isNameChar :: Char -> Bool
isNameChar      c = isAlphaNum c || c `elem` ".-_"

{-|
Convert a filepath to a file: URI stored in a QName. If the
input file path is relative then the working directory is used
to convert it into an absolute path.

If the input represents a directory then it *must* end in 
the directory separator - e.g. "/foo/bar/" rather than "/foo/bar"
for Posix systems.

This has not been tested on Windows.

-}
qnameFromFilePath :: FilePath -> IO QName
qnameFromFilePath = fmap qnameFromURI . filePathToURI
  
filePathToURI :: FilePath -> IO String
filePathToURI fname = do
  ipath <- canonicalizePath fname
  let paths = splitDirectories ipath
      txt = intercalate "/" $ case paths of
        "/":rs -> rs
        _      -> paths
  
  return $ "file:///" ++ txt

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
