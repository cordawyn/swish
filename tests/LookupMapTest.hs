{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  LookupMapTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  FlexibleInstances, FlexibleContexts, MultiParamTypeClasses
--
-- This Module defines test cases for module Parse parsing functions.
--
--------------------------------------------------------------------------------

module Main where

import Data.LookupMap
    ( LookupEntryClass(..), LookupMap(..)
    , makeLookupMap
    , reverseLookupMap
    , mapFind, mapContains
    , mapReplace, mapReplaceAll, mapReplaceMap
    , mapAdd, mapAddIfNew
    , mapDelete, mapDeleteAll
    , mapEq, mapKeys, mapVals
    , mapMerge
    )

import Data.List ( sort )

import Test.HUnit ( Test(TestList) )

import TestHelpers (runTestSuite, testEq)
  
------------------------------------------------------------
--  Declare lookup entry for testing
------------------------------------------------------------

data GenMapEntry a b = E a b

instance (Eq a, Show a, Eq b, Show b)
    => LookupEntryClass (GenMapEntry a b) a b
    where
        keyVal   (E k v) = (k,v)
        newEntry (k,v)   = E k v

instance (Eq a, Show a, Eq b, Show b) => Show (GenMapEntry a b) where
    show = entryShow

instance (Eq a, Show a, Eq b, Show b) => Eq (GenMapEntry a b) where
    (==) = entryEq

type TestEntry  = GenMapEntry Int String
type TestMap    = LookupMap (GenMapEntry Int String)
type RevTestMap = LookupMap (GenMapEntry String Int)
type MayTestMap = Maybe RevTestMap
type StrTestMap = LookupMap (GenMapEntry String String)

------------------------------------------------------------
--  LookupMap functions
------------------------------------------------------------

newMap :: [(Int,String)] -> TestMap
newMap es = makeLookupMap (map newEntry es)

testLookupMap :: String -> TestMap -> [(Int,String)] -> Test
testLookupMap lab m1 m2 = testEq ("LookupMap"++lab ) (newMap m2) m1

testLookupMapFind :: String -> TestMap -> Int -> String -> Test
testLookupMapFind lab lm k res =
    testEq ("LookupMapFind"++lab ) res (mapFind "" k lm)

lm00, lm01, lm02, lm03, lm04, lm05, lm06, lm07, lm08, lm09 :: TestMap
lm00 = newMap []
lm01 = mapAdd lm00 $ newEntry (1,"aaa")
lm02 = mapAdd lm01 $ newEntry (2,"bbb")
lm03 = mapAdd lm02 $ newEntry (3,"ccc")
lm04 = mapAdd lm03 $ newEntry (2,"bbb")
lm05 = mapReplaceAll lm04 $ newEntry (2,"bbb1")
lm06 = mapReplaceAll lm05 $ newEntry (9,"zzzz")
lm07 = mapReplace lm06 $ newEntry (2,"bbb")
lm08 = mapDelete lm07 3
lm09 = mapDeleteAll lm08 2

lm20, lm21, lm22, lm33, lm34, lm35, lm36 :: TestMap
lm20 = mapReplaceMap lm05 $ newMap [(2,"bbb20"),(3,"ccc20")]
lm21 = mapReplaceMap lm05 $ newMap []
lm22 = mapReplaceMap lm05 $ newMap [(9,"zzz22"),(1,"aaa22")]
lm33 = mapAddIfNew lm22 $ newEntry (1,"aaa33")
lm34 = mapAddIfNew lm22 $ newEntry (4,"ddd34")
lm35 = mapReplace lm22 (newEntry (1,"aaa35"))
lm36 = mapReplace lm22 (newEntry (4,"ddd36"))

testLookupMapSuite :: Test
testLookupMapSuite = 
  TestList
  [ testLookupMap     "00" lm00 []
  , testLookupMapFind "00" lm00 2 ""
  , testLookupMap     "01" lm01 [(1,"aaa")]
  , testLookupMapFind "01" lm01 2 ""
  , testLookupMap     "02" lm02 [(2,"bbb"),(1,"aaa")]
  , testLookupMapFind "02" lm02 2 "bbb"
  , testLookupMap     "03" lm03 [(3,"ccc"),(2,"bbb"),(1,"aaa")]
  , testLookupMapFind "03" lm03 2 "bbb"
  , testLookupMap     "04" lm04 [(2,"bbb"),(3,"ccc"),(2,"bbb"),(1,"aaa")]
  , testLookupMapFind "04" lm04 2 "bbb"
  , testLookupMap     "05" lm05 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa")]
  , testLookupMapFind "05" lm05 2 "bbb1"
  , testLookupMap     "06" lm06 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa")]
  , testLookupMapFind "06" lm06 2 "bbb1"
  , testLookupMap     "07" lm07 [(2,"bbb"),(3,"ccc"),(2,"bbb1"),(1,"aaa")]
  , testLookupMapFind "07" lm07 2 "bbb"
  , testLookupMapFind "0x" lm07 9 ""
  , testLookupMap     "08" lm08 [(2,"bbb"),(2,"bbb1"),(1,"aaa")]
  , testLookupMapFind "08" lm08 2 "bbb"
  , testLookupMap     "09" lm09 [(1,"aaa")]
  , testLookupMapFind "09" lm09 2 ""
  , testLookupMap     "20" lm20 [(2,"bbb20"),(3,"ccc20"),(2,"bbb20"),(1,"aaa")]
  , testLookupMapFind "20" lm20 2 "bbb20"
  , testLookupMap     "21" lm21 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa")]
  , testLookupMapFind "21" lm21 2 "bbb1"
  , testLookupMap     "22" lm22 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa22")]
  , testLookupMapFind "22" lm22 1 "aaa22"
  , testEq "LookupContains31" True  (mapContains lm22 2)
  , testEq "LookupContains32" False (mapContains lm22 9)
  , testLookupMap      "33" lm33 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa22")]
  , testLookupMapFind "33a" lm33 1 "aaa22"
  , testLookupMapFind "33b" lm33 4 ""
  , testLookupMap      "34" lm34 [(4,"ddd34"),(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa22")]
  , testLookupMapFind "34a" lm34 1 "aaa22"
  , testLookupMapFind "34b" lm34 4 "ddd34"
  , testLookupMap      "35" lm35 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa35")]
  , testLookupMapFind "35a" lm35 1 "aaa35"
  , testLookupMapFind "35b" lm35 4 ""
  , testLookupMap      "36" lm36 [(2,"bbb1"),(3,"ccc"),(2,"bbb1"),(1,"aaa22"),(4,"ddd36")]
  , testLookupMapFind "36a" lm36 1 "aaa22"
  , testLookupMapFind "36b" lm36 4 "ddd36"
  ]

------------------------------------------------------------
--  Reverse lookup map test tests
------------------------------------------------------------

revdef :: Int
revdef = -1

newRevMap :: [(String,Int)] -> RevTestMap
newRevMap es = makeLookupMap (map newEntry es)

testRevLookupMap :: String -> RevTestMap -> [(String,Int)] -> Test
testRevLookupMap lab m1 m2 =
    testEq ("RevLookupMap"++lab) (newRevMap m2) m1

testRevLookupMapFind :: String -> RevTestMap -> String -> Int -> Test
testRevLookupMapFind lab lm k res =
    testEq ("RevLookupMapFind"++lab) res (mapFind revdef k lm)

rlm00 :: RevTestMap
rlm00 = reverseLookupMap lm00

rlm01 :: RevTestMap
rlm01 = reverseLookupMap lm01

rlm02 :: RevTestMap
rlm02 = reverseLookupMap lm02

rlm03 :: RevTestMap
rlm03 = reverseLookupMap lm03

rlm04 :: RevTestMap
rlm04 = reverseLookupMap lm04

rlm05 :: RevTestMap
rlm05 = reverseLookupMap lm05

rlm06 :: RevTestMap
rlm06 = reverseLookupMap lm06

rlm07 :: RevTestMap
rlm07 = reverseLookupMap lm07

rlm08 :: RevTestMap
rlm08 = reverseLookupMap lm08

rlm09 :: RevTestMap
rlm09 = reverseLookupMap lm09

testRevLookupMapSuite :: Test
testRevLookupMapSuite = 
  TestList
  [ testRevLookupMap     "00" rlm00 []
  , testRevLookupMapFind "00" rlm00 "" revdef
  , testRevLookupMap     "01" rlm01 [("aaa",1)]
  , testRevLookupMapFind "01" rlm01 "bbb" revdef
  , testRevLookupMap     "02" rlm02 [("bbb",2),("aaa",1)]
  , testRevLookupMapFind "02" rlm02 "bbb" 2
  , testRevLookupMap     "03" rlm03 [("ccc",3),("bbb",2),("aaa",1)]
  , testRevLookupMapFind "03" rlm03 "bbb" 2
  , testRevLookupMap     "04" rlm04 [("bbb",2),("ccc",3),("bbb",2),("aaa",1)]
  , testRevLookupMapFind "04" rlm04 "bbb" 2
  , testRevLookupMap     "05" rlm05 [("bbb1",2),("ccc",3),("bbb1",2),("aaa",1)]
  , testRevLookupMapFind "05" rlm05 "bbb1" 2
  , testRevLookupMap     "06" rlm06 [("bbb1",2),("ccc",3),("bbb1",2),("aaa",1)]
  , testRevLookupMapFind "06" rlm06 "bbb1" 2
  , testRevLookupMap     "07" rlm07 [("bbb",2),("ccc",3),("bbb1",2),("aaa",1)]
  , testRevLookupMapFind "07" rlm07 "bbb" 2
  , testRevLookupMapFind "07" rlm07 "bbb1" 2
  , testRevLookupMapFind "0x" rlm07 "*" revdef
  , testRevLookupMap     "08" rlm08 [("bbb",2),("bbb1",2),("aaa",1)]
  , testRevLookupMapFind "08" rlm08 "bbb" 2
  , testRevLookupMap     "09" rlm09 [("aaa",1)]
  , testRevLookupMapFind "09" rlm09 "" revdef
  ]    

------------------------------------------------------------
--  mapKeys
------------------------------------------------------------

testMapKeys :: String -> TestMap -> [Int] -> Test
testMapKeys lab m1 mk =
    testEq ("testMapKeys:"++lab) mk (sort $ mapKeys m1)

testMapKeysSuite :: Test
testMapKeysSuite = 
  TestList
  [ testMapKeys "00" lm00 []
 ,  testMapKeys "01" lm01 [1]
 ,  testMapKeys "02" lm02 [1,2]
 ,  testMapKeys "03" lm03 [1,2,3]
 ,  testMapKeys "04" lm04 [1,2,3]
 ,  testMapKeys "05" lm05 [1,2,3]
 ,  testMapKeys "06" lm06 [1,2,3]
 ,  testMapKeys "07" lm07 [1,2,3]
 ,  testMapKeys "08" lm08 [1,2]
 ,  testMapKeys "09" lm09 [1]
 ]

------------------------------------------------------------
--  mapVals
------------------------------------------------------------

testMapVals :: String -> TestMap -> [String] -> Test
testMapVals lab m1 mv =
    testEq ("MapVals:"++lab) mv (sort $ mapVals m1)

testMapValsSuite :: Test
testMapValsSuite =
  TestList
  [ testMapVals "00" lm00 []
  , testMapVals "01" lm01 ["aaa"]
  , testMapVals "02" lm02 ["aaa","bbb"]
  , testMapVals "03" lm03 ["aaa","bbb","ccc"]
  , testMapVals "04" lm04 ["aaa","bbb","ccc"]
  , testMapVals "05" lm05 ["aaa","bbb1","ccc"]
  , testMapVals "06" lm06 ["aaa","bbb1","ccc"]
  , testMapVals "07" lm07 ["aaa","bbb","bbb1","ccc"]
  , testMapVals "08" lm08 ["aaa","bbb","bbb1"]
  , testMapVals "09" lm09 ["aaa"]
  ]

------------------------------------------------------------
--  mapEq
------------------------------------------------------------

maplist :: [(String, TestMap)]
maplist =
  [ ("lm00",lm00)
  , ("lm01",lm01)
  , ("lm02",lm02)
  , ("lm03",lm03)
  , ("lm04",lm04)
  , ("lm05",lm05)
  , ("lm06",lm06)
  , ("lm07",lm07)
  , ("lm08",lm08)
  , ("lm09",lm09)
  ]

mapeqlist :: [(String, String)]
mapeqlist =
  [ ("lm01","lm09")
  , ("lm02","lm08")
  , ("lm03","lm04")
  , ("lm03","lm07")
  , ("lm04","lm07")
  , ("lm05","lm06")
  ]

testMapEq :: String -> Bool -> TestMap -> TestMap -> Test
testMapEq lab eq m1 m2 =
    testEq ("testMapEq:"++lab) eq (mapEq m1 m2)

testMapEqSuite :: Test
testMapEqSuite = TestList
  [ testMapEq (testLab l1 l2) (nodeTest l1 l2) m1 m2
      | (l1,m1) <- maplist , (l2,m2) <- maplist ]
    where
    testLab l1 l2 = l1 ++ "-" ++ l2
    nodeTest  l1 l2 = (l1 == l2)       ||
            (l1,l2) `elem` mapeqlist ||
            (l2,l1) `elem` mapeqlist

------------------------------------------------------------
--  mapSelect and mapMerge
------------------------------------------------------------

lm101, lm102, lm103, lm104 :: TestMap
lm101 = mapAdd lm03 $ newEntry (4,"ddd")
{-
lm102 = mapSelect lm101 [1,3]
lm103 = mapSelect lm101 [2,4]
lm104 = mapSelect lm101 [2,3]
-}

lm102 = mapAdd (mapAdd (newMap []) (newEntry (1,"aaa"))) $ newEntry (3,"ccc")
lm103 = mapAdd (mapAdd (newMap []) (newEntry (2,"bbb"))) $ newEntry (4,"ddd")
lm104 = mapAdd (mapAdd (newMap []) (newEntry (2,"bbb"))) $ newEntry (3,"ccc")

lm105, lm106, lm107, lm108 :: TestMap
lm105 = mapMerge lm102 lm103
lm106 = mapMerge lm102 lm104
lm107 = mapMerge lm103 lm104
lm108 = mapMerge lm101 lm102

mapMergeSuite :: Test
mapMergeSuite =
  TestList
  [ testLookupMap "101" lm101 [(4,"ddd"),(3,"ccc"),(2,"bbb"),(1,"aaa")]
  , testLookupMap "105" lm105 [(1,"aaa"),(2,"bbb"),(3,"ccc"),(4,"ddd")]
  , testLookupMap "106" lm106 [(1,"aaa"),(2,"bbb"),(3,"ccc")]
  , testLookupMap "107" lm107 [(2,"bbb"),(3,"ccc"),(4,"ddd")]
  , testLookupMap "108" lm108 [(1,"aaa"),(2,"bbb"),(3,"ccc"),(4,"ddd")]
  ] 
  
------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ testLookupMapSuite
  , testRevLookupMapSuite
  , testMapKeysSuite
  , testMapValsSuite
  , testMapEqSuite
  , mapMergeSuite
  ]

main :: IO ()
main = runTestSuite allTests

{-
runTestFile t = do
    h <- openFile "a.tmp" WriteMode
    runTestText (putTextToHandle h False) t
    hClose h
tf = runTestFile
tt = runTestTT
-}

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
