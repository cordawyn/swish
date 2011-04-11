--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFRulesetTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module contains test cases for ruleset data.
--
--  Note that the proof-related methods defined in RDFRuleset are tested
--  by RDFProofTest and/or RDFProofCheck.
--
{--------+---------+---------+---------+---------+---------+---------+---------}

module Main where

import Swish.RDF.RDFRuleset
    ( RDFFormula, RDFRule, RDFClosure, RDFRuleset
    , GraphClosure(..)
    , makeRDFGraphFromN3String
    , makeRDFFormula
    , makeN3ClosureSimpleRule
    , makeNodeAllocTo
    -- for debugging
    , graphClosureFwdApply, graphClosureBwdApply
    )

import Swish.RDF.RDFQuery
    ( rdfQueryBack, rdfQueryBackModify )

import Swish.RDF.RDFVarBinding
    ( RDFVarBinding
    , RDFVarBindingModify
    , RDFVarBindingFilter
    , rdfVarBindingXMLLiteral
    )

import Swish.RDF.RDFGraph
    ( Label (..), RDFLabel(..), RDFGraph
    , Arc(..)
    , getArcs
    , allLabels
    , toRDFGraph
    )
    
import Swish.RDF.VarBinding
    ( makeVarBinding
    , vbmCompose
    , makeVarFilterModify
    )

import Swish.RDF.Ruleset
    ( makeRuleset, getRulesetNamespace, getRulesetAxioms, getRulesetRules
    , getRulesetAxiom, getRulesetRule )

import Swish.RDF.Rule
    ( Formula(..), Rule(..)
    , fwdCheckInference )

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    , makeScopedName
    )

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceOWL
    , scopeRDF
    )

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertBool, assertEqual
    , runTestTT
    )

import Data.List (nub, sort)
import Data.Maybe (isJust, fromJust)

------------------------------------------------------------
--  Test case helpers
------------------------------------------------------------

test :: String -> Bool -> Test
test lab bv =
    TestCase ( assertBool ("test:"++lab) bv )

testVal :: (Eq a, Show a) => String -> a -> a -> Test
testVal lab a1 a2 =
    TestCase ( assertEqual ("testVal:"++lab) a1 a2 )

testEq :: (Eq a, Show a) => String -> Bool -> a -> a -> Test
testEq lab eq a1 a2 =
    TestCase ( assertEqual ("testEq:"++lab) eq (a1==a2) )

testEqual :: (Eq a, Show a) => String -> a -> a -> Test
testEqual lab a1 a2 =
    TestCase ( assertEqual ("testEq:"++lab) a1 a2 )

testLe :: (Ord a, Show a) => String -> Bool -> a -> a -> Test
testLe lab eq a1 a2 =
    TestCase ( assertEqual ("testLe:"++lab) eq (a1<=a2) )

testStringEq :: String -> String -> String -> Test
testStringEq lab s1 s2 =
    TestCase ( assertEqual ("testStringEq:"++lab) s1 s2 )

testSameNamespace :: String -> Namespace -> Namespace -> Test
testSameNamespace lab n1 n2 =
    TestCase ( assertBool ("testSameNamespace:"++lab) ((p1==p2)&&(u1==u2)) )
    where
        p1 = nsPrefix n1
        p2 = nsPrefix n2
        u1 = nsURI n1
        u2 = nsURI n2

testScopedNameEq :: String -> Bool -> ScopedName -> ScopedName -> Test
testScopedNameEq lab eq n1 n2 =
    TestCase ( assertEqual ("testScopedNameEq:"++lab) eq (n1==n2) )

{-
testQNameEq :: String -> Bool -> QName -> QName -> Test
testQNameEq lab eq n1 n2 =
    TestCase ( assertEqual ("testQNameEq:"++lab) eq (n1==n2) )
-}

testSameAxioms :: String -> [RDFFormula] -> [RDFFormula] -> Test
testSameAxioms lab as1 as2 =
    TestCase ( assertBool ("testSameAxioms:"++lab) sameas )
    where
        sameas = (sort as1) == (sort as2)

testSameRules :: String -> [RDFRule] -> [RDFRule] -> Test
testSameRules lab rs1 rs2 =
    TestCase ( assertBool ("testSameRules:"++lab) samers )
    where
        samers = (sort rs1) == (sort rs2)

------------------------------------------------------------
--  Common values
------------------------------------------------------------

pref_rdf, pref_owl :: String
pref_rdf = nsURI namespaceRDF
pref_owl = nsURI namespaceOWL

------------------------------------------------------------
--  Define and manipulate rulesets
------------------------------------------------------------
--
--  A ruleset is essentially a collection of axioms and rules
--  associated with a namespace.
--
--  Rulesets for RDF, RDFS and basic datatyping are predefined:
--  see RDFRuleset, RDFSRuleset and RDFDRuleset.
--  Additional rulesets may be defined for specific datatypes.
--
--  A proof context is a list of rulesets,
--  which may be cited by a proof.

rn1 :: Namespace
rn1  = Namespace "r1" "http://id.ninebynine.org/wip/2003/rulesettest/r1"

-- Common prefix declarations for graph expressions
pref :: String
pref =
    "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . \n" ++
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . \n" ++
    "@prefix ex:   <http://example.org/> . \n" ++
    " \n"

a11, a12 :: RDFFormula
a11  = makeRDFFormula rn1 "a11" (pref++"ex:R1 rdf:type ex:C1 .")
a12  = makeRDFFormula rn1 "a12" (pref++"ex:R2 rdf:type ex:C2 .")

r11, r12 :: RDFRule
r11  = makeN3ClosureSimpleRule rn1 "r11"
            ( pref++"?r1 rdf:type ex:C1 . ?r2 rdf:type ex:C2 ." )
            ( pref++"?r1 ex:P1 ?r2 ." )

r12  = makeN3ClosureSimpleRule rn1 "r12"
            ( pref++"?r1 rdf:type ex:C1 . ?r2 rdf:type ex:C2 ." )
            ( pref++"?r2 ex:P2 ?r1 ." )

--  Basic formula and rule comparison tests
--  (tests support code added in module Proof.hs)

testFormulaSuite :: Test
testFormulaSuite =
  TestList
  [ testEq "testCmpAX01" True  a11 a11
  , testEq "testCmpAX02" False a11 a12
  , testLe "testCmpAX03" True  a11 a11
  , testLe "testCmpAX04" True  a11 a12
  , testLe "testCmpAX05" False a12 a11
  ]

testRuleSuite :: Test
testRuleSuite =
  TestList
  [ testEq "testCmpRU01" True  r11 r11
  , testEq "testCmpRU02" False r11 r12
  , testLe "testCmpRU03" True  r11 r11
  , testLe "testCmpRU04" True  r11 r12
  , testLe "testCmpRU05" False r12 r11
  ]

--  Test simple ruleset construction and access

a1s :: [RDFFormula]
a1s  = [ a11, a12 ]

r1s :: [RDFRule]
r1s  = [ r11, r12 ]

r1 :: RDFRuleset
r1 = makeRuleset rn1 a1s r1s

testRulesetSuite :: Test
testRulesetSuite = 
  TestList
  [ testSameNamespace "testNS01" rn1 (getRulesetNamespace r1)
  , testSameAxioms    "testAX01" a1s (getRulesetAxioms r1)
  , testSameRules     "testRU01" r1s (getRulesetRules r1)
  , testEqual "testGeta11" (Just a11) $
      getRulesetAxiom (ScopedName rn1 "a11") r1
  , testEqual "testGeta11" (Just a12) $
      getRulesetAxiom (ScopedName rn1 "a12") r1
  , testEqual "testGetr11" (Just r11) $
      getRulesetRule (ScopedName rn1 "r11") r1
  , testEqual "testGetr12" (Just r12) $
      getRulesetRule (ScopedName rn1 "r12") r1
  , testEqual "testGetnone" Nothing $
      getRulesetRule (ScopedName rn1 "none") r1
  ]

------------------------------------------------------------
--  Component tests for RDF proof context
------------------------------------------------------------

prefix :: String
prefix =
    "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> . \n" ++
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> . \n" ++
    "@prefix ex:   <http://example.org/> . \n" ++
    " \n"

scopeex :: Namespace
scopeex = Namespace "ex" "http://id.ninebynine.org/wip/2003/RDFProofCheck#"

makeFormula :: Namespace -> String -> String -> RDFFormula
makeFormula scope local gr =
    makeRDFFormula scope local (prefix++gr)

allocateTo :: String -> String -> [RDFLabel] -> RDFVarBindingModify
allocateTo bv av = makeNodeAllocTo (Var bv) (Var av)

isXMLLit :: String -> RDFVarBindingFilter
isXMLLit = rdfVarBindingXMLLiteral . Var

queryBack :: [Arc RDFLabel] -> RDFGraph -> [[RDFVarBinding]]
queryBack qas = rdfQueryBack (toRDFGraph qas) 

-- Backward chaining rdf:r2

rdfr2ant, rdfr2con :: RDFGraph
rdfr2ant  = makeRDFGraphFromN3String "?x ?a ?l . "
rdfr2con  = makeRDFGraphFromN3String "?x ?a ?b . ?b rdf:type rdf:XMLLiteral ."

rdfr2modv :: RDFVarBindingModify
rdfr2modv = allocateTo "b" "l" $ allLabels labelIsVar rdfr2ant

rdfr2modc :: Maybe RDFVarBindingModify
rdfr2modc = vbmCompose (makeVarFilterModify $ isXMLLit "l") rdfr2modv

rdfr2grc :: RDFClosure 
rdfr2grc = GraphClosure
            { nameGraphRule = ScopedName scopeRDF "r2"
            , ruleAnt       = getArcs rdfr2ant
            , ruleCon       = getArcs rdfr2con
            , ruleModify    = fromJust rdfr2modc
            }

rdfr2rul :: RDFRule
rdfr2rul = Rule
            { ruleName       = nameGraphRule rdfr2grc
            , fwdApply       = graphClosureFwdApply rdfr2grc
            , bwdApply       = graphClosureBwdApply rdfr2grc
            , checkInference = fwdCheckInference rdfr2rul
            }

con03 :: RDFGraph
con03 = formExpr $ makeFormula scopeex "con03" $
    "ex:s ex:p1 _:l1 ; ex:p2a _:l2; ex:p2b _:l2 ." ++
    "_:l1 rdf:type rdf:XMLLiteral ." ++
    "_:l2 rdf:type rdf:XMLLiteral ."

v_a, v_b, v_x :: RDFLabel
v_a   = Var "a"
v_b   = Var "b"
v_x   = Var "x"

u_s, u_p1, u_p2a, u_p2b, u_rt, u_rx :: RDFLabel
u_s   = Res $ makeScopedName "" "http://example.org/" "s"
u_p1  = Res $ makeScopedName "" "http://example.org/" "p1"
u_p2a = Res $ makeScopedName "" "http://example.org/" "p2a"
u_p2b = Res $ makeScopedName "" "http://example.org/" "p2b"
u_rt  = Res $ makeScopedName "" pref_rdf "type"
u_rx  = Res $ makeScopedName "" pref_rdf "XMLLiteral"

b_l1, b_l2 :: RDFLabel
b_l1  = Blank "l1"
b_l2  = Blank "l2"

rdfr2v1, rdfr2b1, rdfr2v2, rdfr2v3 :: [[RDFVarBinding]]
rdfr2v1 = queryBack (ruleCon rdfr2grc) con03

rdfr2b1 = [ [ makeVarBinding [ (v_x,u_s), (v_a,u_p1),  (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s), (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s), (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_b,b_l1) ]
            , makeVarBinding [ (v_b,b_l2) ]
            ]
          , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_b,b_l2) ]
            ]
          , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_b,b_l1) ]
            , makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_rx) ]
            ]
          , [ makeVarBinding [ (v_x,u_s),  (v_a,u_p1),  (v_b,b_l1) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2a), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,u_s),  (v_a,u_p2b), (v_b,b_l2) ]
            , makeVarBinding [ (v_x,b_l1), (v_a,u_rt),  (v_b,u_rx) ]
            , makeVarBinding [ (v_x,b_l2), (v_a,u_rt),  (v_b,u_rx) ]
            ]
          ]

rdfr2v2 = rdfQueryBackModify (ruleModify rdfr2grc) rdfr2v1
rdfr2v3 = map nub rdfr2v2

testRDFSuite :: Test
testRDFSuite = 
  TestList
  [ test    "testRDF01" $ isJust rdfr2modc
  , testVal "testRDF02" rdfr2b1 rdfr2v1
  , testVal "testRDF03" [] rdfr2v2
  , testVal "testRDF04" [] rdfr2v3
  , testEq "testRDF09" True [] $ bwdApply rdfr2rul con03
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ testFormulaSuite
  , testRuleSuite
  , testRulesetSuite
  , testRDFSuite
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