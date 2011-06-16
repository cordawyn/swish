{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QNameTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines test cases for QName data
--
--------------------------------------------------------------------------------

module Main where

import Swish.Utils.QName
    ( QName(..)
    , newQName, qnameFromPair, qnameFromURI
    , getNamespace, getLocalName, getQNameURI
    , splitURI
    )

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertEqual
    , runTestTT
    )




------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

base1, base2, base3, base4, base5 :: String
base1  = "http://id.ninebynine.org/wip/2003/test/graph1/node#"
base2  = "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3  = "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4  = "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"
base5  = "http://id.ninebynine.org/wip/2003/test/graph5/"

qb1s1, qb2s2, qb3s3, qb3, qb3bm, qb4m :: QName
qb1s1  = QName base1 "s1"
qb2s2  = QName base2 "s2"
qb3s3  = QName base3 "s3"
qb3    = QName base3 ""
qb3bm  = QName base3 "basemore"
qb4m   = QName base4 "more"

qb5, qb5s5 :: QName
qb5    = QName base5 ""
qb5s5  = QName base5 "s5"

qb1st1, qb2st2, qb3st3 :: QName
qb1st1 = QName base1 "st1"
qb2st2 = QName base2 "st2"
qb3st3 = QName base3 "st3"

------------------------------------------------------------
--  QName equality tests
------------------------------------------------------------

testQNameEq :: String -> Bool -> QName -> QName -> Test
testQNameEq lab eq n1 n2 =
    TestCase ( assertEqual ("testQNameEq:"++lab) eq (n1==n2) )

qnlist :: [(String, QName)]
qnlist =
  [ ("qb1s1", qb1s1)
  , ("qb2s2", qb2s2)
  , ("qb3s3", qb3s3)
  , ("qb3",   qb3)
  , ("qb3bm", qb3bm)
  , ("qb4m",  qb4m)
  , ("qb5",   qb5)
  , ("qb5s5", qb5s5)
  , ("qb1st1",qb1st1)
  , ("qb2st2",qb2st2)
  , ("qb3st3",qb3st3)
  ]

qneqlist :: [(String, String)]
qneqlist =
  [ ("qb3bm","qb4m")
  ]

testQNameEqSuite :: Test
testQNameEqSuite = TestList
  [ testQNameEq (testLab l1 l2) (testEq  l1 l2) n1 n2
      | (l1,n1) <- qnlist , (l2,n2) <- qnlist ]
    where
    testLab l1 l2 = l1 ++ "-" ++ l2
    testEq  l1 l2 = (l1 == l2)        ||
            (l1,l2) `elem` qneqlist ||
            (l2,l1) `elem` qneqlist

------------------------------------------------------------
--  Alternative constructors
------------------------------------------------------------

nq1, nq2 :: QName
nq1 = newQName base1 "s1"
nq2 = newQName base1 "s2"

qp1, qp2 :: QName
qp1 = qnameFromPair (base1,"s1")
qp2 = qnameFromPair (base1,"s2")

qu1, qu2, qu3, qu4, qu5 :: QName
qu1 = qnameFromURI "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
qu2 = qnameFromURI "http://id.ninebynine.org/wip/2003/test/graph2/node/s2"
qu3 = "http://id.ninebynine.org/wip/2003/test/graph3/node"
qu4 = "http://id.ninebynine.org/wip/2003/test/graph5/"
qu5 = "http://id.ninebynine.org/wip/2003/test/graph5/s5"

testMakeQNameSuite :: Test
testMakeQNameSuite = 
  TestList
  [ testQNameEq "testnq01" True  nq1 qb1s1
  , testQNameEq "testnq02" False nq2 qb1s1
  , testQNameEq "testqp01" True  qp1 qb1s1
  , testQNameEq "testqp02" False qp2 qb1s1
  , testQNameEq "testqu01" True qb1s1 qu1
  , testQNameEq "testqu02" True qb2s2 qu2
  , testQNameEq "testqu03" True qb3   qu3
  , testQNameEq "testqu04" True qb5   qu4
  , testQNameEq "testqu05" True qb5s5 qu5
  ]

------------------------------------------------------------
--  Extract components
------------------------------------------------------------

testStringEq :: String -> String -> String -> Test
testStringEq lab s1 s2 =
    TestCase ( assertEqual ("testStringEq:"++lab) s1 s2 )

testPartQNameSuite :: Test
testPartQNameSuite = 
  TestList
  [ testStringEq "testGetNamespace01"
        "http://id.ninebynine.org/wip/2003/test/graph1/node#" 
        (getNamespace qb1s1)
  , testStringEq "testGetNamespace02"
        "http://id.ninebynine.org/wip/2003/test/graph2/node/"
        (getNamespace qb2s2)
  , testStringEq "testGetNamespace03"
        "http://id.ninebynine.org/wip/2003/test/graph3/node"
        (getNamespace qb3s3)
  , testStringEq "testGetNamespace04"
        "http://id.ninebynine.org/wip/2003/test/graph3/node"
        (getNamespace qb3)
  , testStringEq "testGetLocalName01"
        "s1" (getLocalName qb1s1)
  , testStringEq "testGetLocalName02"
        "s2" (getLocalName qb2s2)
  , testStringEq "testGetLocalName03"
      "s3" (getLocalName qb3s3)
  , testStringEq "testGetLocalName04"
      "" (getLocalName qb3)
  , testStringEq "testGetQNameURI01"
      "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
      (getQNameURI qb1s1)
  , testStringEq "testGetQNameURI02"
      "http://id.ninebynine.org/wip/2003/test/graph2/node/s2"
      (getQNameURI qb2s2)
  , testStringEq "testGetQNameURI03"
      "http://id.ninebynine.org/wip/2003/test/graph3/nodes3"
      (getQNameURI qb3s3)
  , testStringEq "testGetQNameURI04"
      "http://id.ninebynine.org/wip/2003/test/graph3/node"
      (getQNameURI qb3)
  ]

------------------------------------------------------------
--  Maybe Qname comparison
------------------------------------------------------------

testMaybeQNameEq :: String -> Bool -> Maybe QName -> Maybe QName -> Test
testMaybeQNameEq lab eq n1 n2 =
    TestCase ( assertEqual ("testMaybeQNameEq:"++lab) eq (n1==n2) )

testMaybeQNameEqSuite :: Test
testMaybeQNameEqSuite = 
  TestList
  [ testMaybeQNameEq "testMaybeQNameEq01" True
      (Just qb1s1) (Just qb1s1)
  , testMaybeQNameEq "testMaybeQNameEq02" False
      (Just qb1s1) (Just qb2s2)
  , testMaybeQNameEq "testMaybeQNameEq03" False
      (Just qb1s1) Nothing
  , testMaybeQNameEq "testMaybeQNameEq04" False
      Nothing (Just qb1s1)
  , testMaybeQNameEq "testMaybeQNameEq05" True
      Nothing Nothing
  ]

------------------------------------------------------------
--  QName ordering
------------------------------------------------------------

testQNameLe :: String -> Bool -> QName -> QName -> Test
testQNameLe lab le n1 n2 =
    TestCase ( assertEqual ("testQNameLe:"++lab) le (n1<=n2) )

testQNameLeSuite :: Test
testQNameLeSuite = 
  TestList
  [testQNameLe "testQNameLe01" True  qb3bm qb4m
  , testQNameLe "testQNameLe02" True  qb4m  qb3bm
  , testQNameLe "testQNameLe03" True  qb1s1 qb2s2
  , testQNameLe "testQNameLe04" False qb2s2 qb1s1
  ]
  
------------------------------------------------------------
--  Show QName
------------------------------------------------------------

testShowQNameSuite :: Test
testShowQNameSuite = 
  TestList
  [testStringEq "testShowQName01"
      "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1>"
      (show qb1s1)
  , testStringEq "testShowQName02"
    "<http://id.ninebynine.org/wip/2003/test/graph2/node/s2>"
    (show qb2s2)
  , testStringEq "testShowQName03"
    "<http://id.ninebynine.org/wip/2003/test/graph3/node>"
    (show qb3)
  , testStringEq "testShowQName04"
    "<http://id.ninebynine.org/wip/2003/test/graph5/>"
    (show qb5)
  ]

------------------------------------------------------------
--  Split URI string into QName parts
------------------------------------------------------------

-- splitURI :: String -> ( String, String )
    -- splitURI "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
    -- splitURI "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
    -- splitURI "http://example.org/aaa/"    = ("http://example.org/aaa/","")

testSplitURI :: String -> String -> ( String, String ) -> Test
testSplitURI label input ans =
    TestCase ( assertEqual label ans ( splitURI input ) )

testSplitURISuite :: Test
testSplitURISuite = 
  TestList
  [ testSplitURI "testSplitURI01"
      "http://example.org/aaa#bbb"
      ( "http://example.org/aaa#", "bbb" )
  , testSplitURI "testSplitURI02"
     "http://example.org/aaa/bbb"
     ( "http://example.org/aaa/", "bbb" )
  , testSplitURI "testSplitURI03"
     "http://example.org/aaa#"
     ( "http://example.org/aaa#", "" )
  , testSplitURI "testSplitURI04"
     "http://example.org/aaa/"
     ( "http://example.org/aaa/", "" )
  , testSplitURI "testSplitURI05"
     "//example.org/aaa#bbb"
     ( "//example.org/aaa#", "bbb" )
  , testSplitURI "testSplitURI06"
     "aaa/bbb"
     ( "aaa/", "bbb" )
  , testSplitURI "testSplitURI07"
     "aaa/bbb/"
     ( "aaa/bbb/", "" )
     
     -- Thanks to Ian Dickinson of the HP Jena team for spotting this one:
     -- So what *is* the correct split here?
  , testSplitURI "testSplitURI08"
      "mortal"
      ( "", "mortal" )
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ testQNameEqSuite
  , testMakeQNameSuite
  , testPartQNameSuite
  , testMaybeQNameEqSuite
  , testQNameLeSuite
  , testShowQNameSuite
  , testSplitURISuite
  ]

main :: IO ()
main = runTestTT allTests >> return ()

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
