--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  TestHelpers
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module contains test case helper functions, providing a range of
--  commonly-used test cases.
--
--------------------------------------------------------------------------------

module TestHelpers
       ( 
         runTestSuite
         , test
         , testCompare, testCompareEq
         , testEq, testNe, testLe, testGe
         , testElem
         , testJust, testNothing 
         , testJe, testJl, testNo
         , testEqv, testEqv2, testHasEqv, testMaybeEqv
         , testMaker                                 
         )
       where

import System.Exit (exitSuccess, exitFailure)

import Test.HUnit (Test(TestCase, TestList)
                   , runTestTT
                   , Assertion
                   , assertBool, assertEqual, assertFailure
                   )
import Test.HUnit.Base (Counts(..))

import Control.Monad (unless)

import Data.Maybe (isJust, isNothing, fromJust)

import Swish.Utils.ListHelpers (equiv)

-- | Run the test suite and exit the process with either 
-- @exitSuccess@ or @exitFailure@.
runTestSuite :: Test -> IO ()
runTestSuite tests = do
  Counts ncases ntried nerrors nfailures <- runTestTT tests
  -- It's not clear whether all these checks are needed but leave in
  if ntried == ncases && nerrors == nfailures && nerrors == 0
    then exitSuccess
    else exitFailure

------------------------------------------------------------
--  Test case helpers
------------------------------------------------------------

assertMember :: (Eq a, Show a) => String -> a -> [a] -> Assertion
assertMember preface expected actual =
  unless (expected `elem` actual ) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\nbut got: " ++ show actual

test :: String -> Bool -> Test
test lab = TestCase . assertBool ("test:"++lab)

testCompare :: (Eq a, Show a) => String -> String -> a -> a -> Test
testCompare typ lab a1 a2 =
    TestCase ( assertEqual (typ++lab) a1 a2 )

testCompareEq :: (Eq a, Show a) => String -> String -> Bool -> a -> a -> Test
testCompareEq typ lab eq a1 a2 =
    TestCase ( assertEqual (typ++lab) eq (a1==a2) )

testMaker :: (Show b, Eq b) => (a -> b) -> String -> String -> a -> a -> Test
testMaker conv l1 l2 x y =
  TestCase (assertEqual (l1 ++ ":" ++ l2) (conv x) (conv y))

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq = testCompare "testEq:"

testNe :: (Eq a, Show a) => String -> a -> a -> Test
testNe lab a1 a2 =
    TestCase ( assertBool ("testNe:"++lab) (a1 /= a2) )

testLe :: (Ord a, Show a) => String -> Bool -> a -> a -> Test
testLe lab eq a1 a2 =
    TestCase ( assertEqual ("testLe:"++lab) eq (a1<=a2) )

testGe :: (Ord a, Show a) => String -> Bool -> a -> a -> Test
testGe lab eq a1 a2 =
    TestCase ( assertEqual ("testGe:"++lab) eq (a1>=a2) )

-- Test for Just x or Nothing

testJust :: String -> Maybe a -> Test
testJust lab = TestCase . assertBool ("testJust:"++lab) . isJust

testNothing :: String -> Maybe a -> Test
testNothing lab = TestCase . assertBool ("testJust:"++lab) . isNothing

testJe :: (Eq a, Show a) => String -> a -> Maybe a -> Test
testJe lab e a = TestList
    [ TestCase $ assertBool  lab (isJust a)
    , TestCase $ assertEqual lab e (fromJust a)
    ]

testJl :: (Eq a, Show a) => String -> Int -> Maybe [a] -> Test
testJl lab e a = TestList
    [ TestCase $ assertBool  lab   (isJust a)
    , TestCase $ assertEqual lab e (length (fromJust a))
    ]

testNo :: (Eq a, Show a) => String -> [[a]] -> Test
testNo lab a =
    TestCase $ assertBool  lab   (null a)

-- Test for list membership

testElem :: (Eq a, Show a) => String -> a -> [a] -> Test
testElem lab a1 as = TestCase ( assertMember ("testElem:"++lab) a1 as )

-- Compare lists and lists of lists and Maybe lists for set equivalence:

data ListTest a = ListTest [a]

instance (Eq a) => Eq (ListTest a) where
    (ListTest a1) == (ListTest a2) = a1 `equiv` a2

instance (Show a) => Show (ListTest a) where
    show (ListTest a) = show a

data MaybeListTest a = MaybeListTest (Maybe [a])

instance (Eq a) => Eq (MaybeListTest a) where
    MaybeListTest (Just a1) == MaybeListTest (Just a2) = a1 `equiv` a2
    MaybeListTest Nothing   == MaybeListTest Nothing   = True
    _                       == _                       = False

instance (Show a) => Show (MaybeListTest a) where
    show (MaybeListTest a) = show a

testEqv :: (Eq a, Show a) => String -> [a] -> [a] -> Test
testEqv = testMaker ListTest "testEqv" 

testEqv2 :: (Eq a, Show a) => String -> [[a]] -> [[a]] -> Test
testEqv2 = testMaker (ListTest . map ListTest) "testEqv2"

testHasEqv :: (Eq a, Show a) => String -> [a] -> [[a]] -> Test
testHasEqv lab a1 a2 =
    TestCase ( assertMember ("testHasEqv:"++lab) ma1 ma2 )
    where
        ma1 = ListTest a1
        ma2 = map ListTest a2

testMaybeEqv :: (Eq a, Show a) => String -> Maybe [a] -> Maybe [a] -> Test
testMaybeEqv = testMaker MaybeListTest "testMaybeEqv"

{-

------------------------------------------------------------
--  Test suites for the above
------------------------------------------------------------

testSuccessSuite = TestList
    [ test          "01" True
    , testEq        "02" 2 2
    , testLe        "03" 1 2
    , testLe        "04" 2 2
    , testGe        "05" 3 2
    , testGe        "07" 2 2
    , testJust      "08" (Just "08")
    , testNothing   "09" (Nothing :: Maybe String)
    , testElem      "10" 'b' "abc"
    , testEqv       "11" "abc" "bca"
    , testEqv       "12" "abc" "bbccaa"
    , testEqv2      "13" ["abc","def","ghi"] ["fed","ghi","bca"]
    , testHasEqv    "14" "abc"               ["fed","ghi","bca"]
    , testHasEqv    "15" "ghi"               ["fed","ghi","bca"]
    , testHasEqv    "16" "def"               ["fed","ghi","bca"]
    , testMaybeEqv  "17" (Just "abc") (Just "bca")
    , testMaybeEqv  "18" Nothing      (Nothing :: Maybe String)
    ]

-- All of these tests should be failures:
-- Look for number of failures == total number of tests
testFailureSuite = TestList
    [ test          "01" False
    , testEq        "02" 2 22
    , testLe        "03" 2 1
    , testGe        "04" 2 3
    , testJust      "05" (Nothing :: Maybe String)
    , testNothing   "06" (Just "09")
    , testElem      "07" 'd' "abc"
    , testEqv       "08" "abd" "bca"
    , testEqv2      "09" ["abd","def","ghi"] ["fed","ghi","bca"]
    , testHasEqv    "10" "abd"               ["fed","ghi","bca"]
    , testMaybeEqv  "11" (Just "abc") (Just "bda")
    , testMaybeEqv  "12" Nothing      (Just "bda")
    ]
-}

------------------------------------------------------------
--  All tests
------------------------------------------------------------

{-
allSuccessTests = TestList
    [ testSuccessSuite
    ]

allFailureTests = TestList
    [ testFailureSuite
    ]

mainS = runTestTT allSuccessTests
mainF = runTestTT allFailureTests
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
