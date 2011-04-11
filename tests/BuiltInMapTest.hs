--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  BuiltInMapTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module contains test cases for accessing built-in variable
--  binding modifiers.
--
--------------------------------------------------------------------------------

module Main where

import Swish.RDF.BuiltInMap
    ( findRDFOpenVarBindingModifier
    , findRDFDatatype
    , rdfRulesetMap
    , allRulesets
    )

import Swish.RDF.RDFDatatypeXsdInteger
    ( typeNameXsdInteger, namespaceXsdInteger
    )

import Swish.RDF.Ruleset
    ( getMaybeContextAxiom
    , getMaybeContextRule
    )

import Swish.Utils.Namespace
    ( ScopedName(..) )

import Swish.RDF.Vocabulary
    ( swishName
    , scopeRDF
    , scopeRDFS
    , scopeRDFD
    , namespaceXsdType
    )

import Swish.Utils.LookupMap
    ( mapFindMaybe )

import Swish.Utils.ListHelpers
    ( equiv )

import Test.HUnit
    ( Test(TestCase,TestList)
    , Assertion
    , assertBool, assertEqual, assertFailure
    , runTestTT, runTestText, putTextToHandle
    )

import System.IO
    ( IOMode(WriteMode)
    , openFile, hClose
    )

import Control.Monad (unless)
import Data.Maybe (isJust)


------------------------------------------------------------
--  Test case helpers
------------------------------------------------------------

assertMember :: (Eq a, Show a) => String -> a -> [a] -> Assertion
assertMember preface expected actual =
  unless (expected `elem` actual ) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\nbut got: " ++ show actual

test :: String -> Bool -> Test
test lab bv =
    TestCase ( assertBool ("test:"++lab) bv )

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq lab a1 a2 =
    TestCase ( assertEqual ("testEq:"++lab) a1 a2 )

testElem :: (Eq a, Show a) => String -> a -> [a] -> Test
testElem lab a1 as =
    TestCase ( assertMember ("testElem:"++lab) a1 as )

testLe :: (Ord a, Show a) => String -> Bool -> a -> a -> Test
testLe lab eq a1 a2 =
    TestCase ( assertEqual ("testLe:"++lab) eq (a1<=a2) )

-- Test for Just x or Nothing

testJust :: String -> Maybe a -> Test
testJust lab av =
    TestCase ( assertBool ("testJust:"++lab) (isJust av) )

testNothing :: String -> Maybe a -> Test
testNothing lab av =
    TestCase ( assertBool ("testJust:"++lab) (not $ isJust av) )

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
testEqv lab a1 a2 =
    TestCase ( assertEqual ("testEqv:"++lab) (ListTest a1) (ListTest a2) )

testEqvEqv :: (Eq a, Show a) => String -> [[a]] -> [[a]] -> Test
testEqvEqv lab a1 a2 =
    TestCase ( assertEqual ("testEqvEqv:"++lab) ma1 ma2 )
    where
        ma1 = ListTest $ map ListTest a1
        ma2 = ListTest $ map ListTest a2

testHasEqv :: (Eq a, Show a) => String -> [a] -> [[a]] -> Test
testHasEqv lab a1 a2 =
    TestCase ( assertMember ("testHasEqv:"++lab) ma1 ma2 )
    where
        ma1 = ListTest a1
        ma2 = map ListTest a2

testMaybeEqv :: (Eq a, Show a) => String -> Maybe [a] -> Maybe [a] -> Test
testMaybeEqv lab a1 a2 =
    TestCase ( assertEqual ("testMaybeEqv:"++lab) ma1 ma2 )
    where
        ma1 = (MaybeListTest a1)
        ma2 = (MaybeListTest a2)

------------------------------------------------------------
--  Test finding built-in variable binding modifiers
------------------------------------------------------------

testVarMod01, testVarMod02, testVarMod03, testVarMod04, 
  testVarMod05, testVarMod06, testVarMod07 :: Test

testVarMod01 = testJust "testVarMod01" $
    findRDFOpenVarBindingModifier (swishName "rdfVarBindingUriRef")
testVarMod02 = testJust "testVarMod02" $
    findRDFOpenVarBindingModifier (swishName "rdfVarBindingDatatyped")
testVarMod03 = testJust "testVarMod03" $
    findRDFOpenVarBindingModifier (swishName "varFilterNE")
testVarMod04 = testJust "testVarMod04" $
    findRDFOpenVarBindingModifier (swishName "nullVarBindingModify")
testVarMod05 = testJust "testVarMod05" $
    findRDFOpenVarBindingModifier (ScopedName namespaceXsdInteger "abs")
testVarMod06 = testJust "testVarMod06" $
    findRDFOpenVarBindingModifier (ScopedName namespaceXsdInteger "divmod")
testVarMod07 = testJust "testVarMod07" $
    findRDFOpenVarBindingModifier (ScopedName namespaceXsdInteger "ge")

testVarModSuite :: Test
testVarModSuite = TestList
    [ testVarMod01, testVarMod02, testVarMod03, testVarMod04
    , testVarMod05, testVarMod06, testVarMod07
    ]

------------------------------------------------------------
--  Test finding built-in datatypes
------------------------------------------------------------

testDatatype01 :: Test
testDatatype01 = testJust "testDatatype01" $ findRDFDatatype typeNameXsdInteger

testDatatypeSuite :: Test
testDatatypeSuite = TestList
    [ testDatatype01
    ]

------------------------------------------------------------
--  Test finding built-in rulesets
------------------------------------------------------------

testRuleset01 :: Test
testRuleset01 = testJust "testRuleset01" $
    mapFindMaybe scopeRDF rdfRulesetMap

testRulesetSuite :: Test
testRulesetSuite = TestList
    [ testRuleset01
    ]

------------------------------------------------------------
--  Test finding arbitrary axioms and rules
------------------------------------------------------------

testFindAxiom01, testFindAxiom02, testFindAxiom03 :: Test

testFindAxiom01 = testJust "testFindAxiom01" $
    getMaybeContextAxiom (ScopedName scopeRDF "a1") allRulesets
testFindAxiom02 = testJust "testFindAxiom02" $
    getMaybeContextAxiom (ScopedName scopeRDFS "a01") allRulesets
testFindAxiom03 = testJust "testFindAxiom03" $
    getMaybeContextAxiom (ScopedName (namespaceXsdType "integer") "dt")
        allRulesets

testFindAxiomSuite :: Test
testFindAxiomSuite = TestList
    [ testFindAxiom01, testFindAxiom02, testFindAxiom03
    ]

testFindRule01, testFindRule02, testFindRule03, testFindRule04 :: Test

testFindRule01 = testJust "testFindRule01" $
    getMaybeContextRule (ScopedName scopeRDF "r1") allRulesets
testFindRule02 = testJust "testFindRule02" $
    getMaybeContextRule (ScopedName scopeRDFS "r1") allRulesets
testFindRule03 = testJust "testFindRule03" $
    getMaybeContextRule (ScopedName scopeRDFD "r1") allRulesets
testFindRule04 = testJust "testFindRule04" $
    getMaybeContextRule (ScopedName (namespaceXsdType "integer") "Abs")
        allRulesets

testFindRuleSuite :: Test
testFindRuleSuite = TestList
    [ testFindRule01, testFindRule02, testFindRule03, testFindRule04
    ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
    [ testVarModSuite
    , testDatatypeSuite
    , testRulesetSuite
    , testFindAxiomSuite
    , testFindRuleSuite
    ]

main :: IO ()
main = runTestTT allTests >> return ()

runTestFile :: Test -> IO ()
runTestFile t = do
    h <- openFile "a.tmp" WriteMode
    _ <- runTestText (putTextToHandle h False) t
    hClose h
    
{-
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