--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFProofTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module tests the RDFproof module, which instantiates the proof
--  rule class over RDF graphs.
--
--------------------------------------------------------------------------------

module Main where

import Swish.RDF.RDFProof
    ( makeRdfInstanceEntailmentRule
    , makeRdfSubgraphEntailmentRule
    , makeRdfSimpleEntailmentRule
    )

import Swish.RDF.RDFQuery
    ( rdfQueryFind, rdfQuerySubs )

import Swish.RDF.RDFVarBinding
    ( RDFVarBinding, RDFVarBindingModify )

import Swish.RDF.RDFRuleset
    ( RDFRule
    , makeRDFGraphFromN3String
    , makeN3ClosureAllocatorRule
    , makeN3ClosureRule
    , makeN3ClosureSimpleRule
    , makeNodeAllocTo
    )

import Swish.RDF.RDFGraph
    ( Label(..), RDFLabel(..), RDFGraph
    , add, allLabels, allNodes )

import Swish.RDF.VarBinding
    ( VarBinding(..) 
    , VarBindingModify(..)
    , makeVarFilterModify
    , varBindingId -- , varFilterDisjunction, varFilterConjunction
    , varFilterNE
    )

import Swish.RDF.Rule (Rule(..))

import Swish.Utils.Namespace
    ( Namespace(..), ScopedName(..) )

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertBool, assertEqual
    , runTestTT )

import Data.Maybe (isJust, fromJust)

--  misc helpers

test :: String -> Bool -> Test
test lab tst = TestCase $ assertBool lab tst

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq lab e a = TestCase $ assertEqual lab e a

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

testIn :: (Eq a, Show a) => String -> a -> [a] -> Test
testIn lab eg a = TestCase $ assertBool lab (eg `elem` a)

--  test1:  simple query with URI, literal and blank nodes.

scope1 :: Namespace
scope1 = Namespace "scope1"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope1"

prefix1 :: String
prefix1 =
    "@prefix ex: <http://example.org/> . \n" ++
    " \n"

graph1 :: RDFGraph
graph1    = makeRDFGraphFromN3String graph1str

graph1str :: String
graph1str = prefix1 ++
    "ex:s1  ex:p  ex:o1 . \n"  ++
    "ex:s2  ex:p  \"lit1\" . \n" ++
    "[ ex:p ex:o3 ] . \n"

query11 :: RDFGraph
query11    = makeRDFGraphFromN3String query11str

query11str :: String
query11str = prefix1 ++
    "?s  ex:p  ?o . \n"

result11 :: RDFGraph
result11    = makeRDFGraphFromN3String result11str

result11str :: String
result11str = prefix1 ++
    "?s  ex:r  ?o . \n"

result11a :: RDFGraph
result11a    = makeRDFGraphFromN3String result11astr

result11astr :: String
result11astr = prefix1 ++
    "ex:s1  ex:r  ex:o1    . \n" ++
    "ex:s2  ex:r  \"lit1\" . \n" ++
    "[ ex:r ex:o3 ]        . \n"

result11b :: RDFGraph
result11b    = makeRDFGraphFromN3String result11bstr

result11bstr :: String
result11bstr = prefix1 ++
    "ex:s1  ex:r  ex:o1    . \n"

result11c :: RDFGraph
result11c    = makeRDFGraphFromN3String result11cstr

result11cstr :: String
result11cstr = prefix1 ++
    "ex:s2  ex:r  \"lit1\" . \n"

backsub11a :: RDFGraph
backsub11a    = makeRDFGraphFromN3String backsub11astr

backsub11astr :: String
backsub11astr = prefix1 ++
    "ex:s1  ex:p  ex:o1    . \n" ++
    "ex:s2  ex:p  \"lit1\" . \n"

backsub11b :: RDFGraph
backsub11b    = makeRDFGraphFromN3String backsub11bstr

backsub11bstr :: String
backsub11bstr = prefix1 ++
    "ex:s2  ex:p  \"lit1\" . \n"

rul11 :: RDFRule
rul11 = makeN3ClosureSimpleRule scope1 "rul11" query11str result11str

fwd11 :: [RDFGraph]
fwd11 = fwdApply rul11 [graph1]

bwd11 :: [[RDFGraph]]
bwd11 = bwdApply rul11 (add result11b result11c)

test1 :: Test
test1 = 
  TestList
  [ testEq "testFwd11" 1 (length fwd11)
  , testIn "testFwd11a" result11a fwd11
  , testEq "testBwd11"  1 (length (head bwd11))
  , testIn "testBwd11a" backsub11a (head bwd11)
  ]

--  test2:  a range of more complex queries based on a
--  single relationship graph.

scope2 :: Namespace
scope2 = Namespace "scope2"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope2"

prefix2 :: String
prefix2 =
    "@prefix pers: <urn:pers:> . \n"      ++
    "@prefix rel:  <urn:rel:> . \n"       ++
    " \n"

graph2 :: RDFGraph
graph2    = makeRDFGraphFromN3String graph2str

graph2str :: String
graph2str = prefix2 ++
    "pers:St1 rel:wife     pers:Do1 ; \n" ++
    "         rel:daughter pers:Ma2 ; \n" ++
    "         rel:daughter pers:An2 . \n" ++
    "pers:Pa2 rel:wife     pers:Ma2 ; \n" ++
    "         rel:son      pers:Gr3 ; \n" ++
    "         rel:son      pers:La3 ; \n" ++
    "         rel:son      pers:Si3 ; \n" ++
    "         rel:son      pers:Al3 . \n" ++
    "pers:Br2 rel:wife     pers:Ri2 ; \n" ++
    "         rel:daughter pers:Ma3 ; \n" ++
    "         rel:son      pers:Wi3 . \n" ++
    "pers:Gr3 rel:wife     pers:Ma3 ; \n" ++
    "         rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter pers:Rh4 . \n" ++
    "pers:Si3 rel:wife     pers:Jo3 ; \n" ++
    "         rel:son      pers:Ol4 ; \n" ++
    "         rel:son      pers:Lo4 . \n" ++
    "pers:Al3 rel:wife     pers:Su3 ; \n" ++
    "         rel:son      pers:Ha4 ; \n" ++
    "         rel:son      pers:El4 . \n"

query21 :: RDFGraph
query21    = makeRDFGraphFromN3String query21str

query21str :: String
query21str = prefix2 ++
    "?a rel:wife ?b . \n"

result21 :: RDFGraph
result21    = makeRDFGraphFromN3String result21str

result21str :: String
result21str = prefix2 ++
    "?b rel:husband ?a . \n"

result21a :: RDFGraph
result21a    = makeRDFGraphFromN3String result21astr

result21astr :: String
result21astr = prefix2 ++
    "pers:Do1 rel:husband pers:St1 . \n" ++
    "pers:Ma2 rel:husband pers:Pa2 . \n" ++
    "pers:Ri2 rel:husband pers:Br2 . \n" ++
    "pers:Ma3 rel:husband pers:Gr3 . \n" ++
    "pers:Jo3 rel:husband pers:Si3 . \n" ++
    "pers:Su3 rel:husband pers:Al3 . \n"

result21b :: RDFGraph
result21b    = makeRDFGraphFromN3String result21bstr

result21bstr :: String
result21bstr = prefix2 ++
    "pers:Do1 rel:husband pers:St1 . \n" ++
    "pers:Ma2 rel:husband pers:Pa2 . \n"

bwd21a :: RDFGraph
bwd21a    = makeRDFGraphFromN3String bwd21astr

bwd21astr :: String
bwd21astr = prefix2 ++
    "pers:St1 rel:wife     pers:Do1 . \n" ++
    "pers:Pa2 rel:wife     pers:Ma2 . \n"

rul21 :: RDFRule
rul21 = makeN3ClosureSimpleRule scope2 "rul21" query21str result21str

fwd21 :: [RDFGraph]
fwd21 = fwdApply rul21 [graph2]

bwd21 :: [[RDFGraph]]
bwd21 = bwdApply rul21 result21b

query22 :: RDFGraph
query22    = makeRDFGraphFromN3String query22str

query22str :: String
query22str = prefix2 ++
    "?a rel:son ?b . \n" ++
    "?b rel:son ?c . \n"

result22 :: RDFGraph
result22    = makeRDFGraphFromN3String result22str

result22str :: String
result22str = prefix2 ++
    "?a rel:grandparent ?c . \n"

result22a :: RDFGraph
result22a    = makeRDFGraphFromN3String result22astr

result22astr :: String
result22astr = prefix2 ++
    "pers:Pa2 rel:grandparent pers:Ro4 . \n" ++
    "pers:Pa2 rel:grandparent pers:Ol4 . \n" ++
    "pers:Pa2 rel:grandparent pers:Lo4 . \n" ++
    "pers:Pa2 rel:grandparent pers:Ha4 . \n" ++
    "pers:Pa2 rel:grandparent pers:El4 . \n"

result22b :: RDFGraph
result22b    = makeRDFGraphFromN3String result22bstr

result22bstr :: String
result22bstr = prefix2 ++
    "pers:Pa2 rel:grandparent pers:Ro4 . \n" ++
    "pers:Pa2 rel:grandparent pers:Ol4 . \n"

bwd22a :: RDFGraph
bwd22a    = makeRDFGraphFromN3String bwd22astr

bwd22astr :: String
bwd22astr = prefix2 ++
    "pers:Pa2 rel:son      _:p1 . \n" ++
    "_:p1 rel:son      pers:Ro4 . \n" ++
    "pers:Pa2 rel:son      _:p2 . \n" ++
    "_:p2 rel:son      pers:Ol4 . \n"

rul22 :: RDFRule
rul22 = makeN3ClosureSimpleRule scope2 "rul22" query22str result22str

fwd22 :: [RDFGraph]
fwd22 = fwdApply rul22 [graph2]

bwd22 :: [[RDFGraph]]
bwd22 = bwdApply rul22 result22b

query23 :: RDFGraph
query23    = makeRDFGraphFromN3String query23str

query23str :: String
query23str = prefix2 ++
    "?a rel:son ?b . \n" ++
    "?a rel:son ?c . \n"

result23 :: RDFGraph
result23    = makeRDFGraphFromN3String result23str

result23str :: String
result23str = prefix2 ++
    "?b rel:brother ?c . \n"

result23a :: RDFGraph
result23a    = makeRDFGraphFromN3String result23astr

result23astr :: String
result23astr = prefix2 ++
    "pers:Gr3 rel:brother pers:Gr3 . \n" ++
    "pers:Gr3 rel:brother pers:La3 . \n" ++
    "pers:Gr3 rel:brother pers:Si3 . \n" ++
    "pers:Gr3 rel:brother pers:Al3 . \n" ++
    "pers:La3 rel:brother pers:Gr3 . \n" ++
    "pers:La3 rel:brother pers:La3 . \n" ++
    "pers:La3 rel:brother pers:Si3 . \n" ++
    "pers:La3 rel:brother pers:Al3 . \n" ++
    "pers:Si3 rel:brother pers:Gr3 . \n" ++
    "pers:Si3 rel:brother pers:La3 . \n" ++
    "pers:Si3 rel:brother pers:Si3 . \n" ++
    "pers:Si3 rel:brother pers:Al3 . \n" ++
    "pers:Al3 rel:brother pers:Gr3 . \n" ++
    "pers:Al3 rel:brother pers:La3 . \n" ++
    "pers:Al3 rel:brother pers:Si3 . \n" ++
    "pers:Al3 rel:brother pers:Al3 . \n" ++
    "pers:Wi3 rel:brother pers:Wi3 . \n" ++
    "pers:Ro4 rel:brother pers:Ro4 . \n" ++
    "pers:Ol4 rel:brother pers:Lo4 . \n" ++
    "pers:Ol4 rel:brother pers:Ol4 . \n" ++
    "pers:Lo4 rel:brother pers:Lo4 . \n" ++
    "pers:Lo4 rel:brother pers:Ol4 . \n" ++
    "pers:Ha4 rel:brother pers:El4 . \n" ++
    "pers:Ha4 rel:brother pers:Ha4 . \n" ++
    "pers:El4 rel:brother pers:El4 . \n" ++
    "pers:El4 rel:brother pers:Ha4 . \n"

result23b :: RDFGraph
result23b    = makeRDFGraphFromN3String result23bstr

result23bstr :: String
result23bstr = prefix2 ++
    "pers:Gr3 rel:brother pers:Gr3 . \n" ++
    "pers:Gr3 rel:brother pers:La3 . \n"

bwd23a :: RDFGraph
bwd23a    = makeRDFGraphFromN3String bwd23astr

bwd23astr :: String
bwd23astr = prefix2 ++
    "_:a1 rel:son pers:Gr3 . \n" ++
    "_:a1 rel:son pers:Gr3 . \n" ++
    "_:a2 rel:son pers:Gr3 . \n" ++
    "_:a2 rel:son pers:La3 . \n"

rul23 :: RDFRule
rul23 = makeN3ClosureSimpleRule scope2 "rul23" query23str result23str

fwd23 :: [RDFGraph]
fwd23 = fwdApply rul23 [graph2]

bwd23 :: [[RDFGraph]]
bwd23 = bwdApply rul23 result23b

--  Test case to return multiple alternative bindings
--
--  (?c son ?a, ?c stepSon b) => (?a stepBrother ?b, ?b stepBrother ?a)
--
--  a stepBrother b if
--      (_:c1 son a, _:c1 stepSon b) || (_:c2 stepSon a, _:c2 son b)

graph24 :: RDFGraph
graph24    = makeRDFGraphFromN3String graph24str

graph24str :: String
graph24str = prefix2 ++
    "pers:Ma2 rel:son     pers:Gr3 . \n" ++
    "pers:Ma2 rel:stepson pers:St3 . \n"

query24 :: RDFGraph
query24    = makeRDFGraphFromN3String query24str

query24str :: String
query24str = prefix2 ++
    "?c rel:son ?a     . \n" ++
    "?c rel:stepson ?b . \n"

result24 :: RDFGraph
result24    = makeRDFGraphFromN3String result24str

result24str :: String
result24str = prefix2 ++
    "?a rel:stepbrother ?b . \n" ++
    "?b rel:stepbrother ?a . \n"

result24a :: RDFGraph
result24a    = makeRDFGraphFromN3String result24astr

result24astr :: String
result24astr = prefix2 ++
    "pers:Gr3 rel:stepbrother pers:St3 . \n" ++
    "pers:St3 rel:stepbrother pers:Gr3 . \n"

bwd24a1 :: RDFGraph
bwd24a1    = makeRDFGraphFromN3String bwd24a1str

bwd24a1str :: String
bwd24a1str = prefix2 ++
    "_:c1 rel:son     pers:Gr3 . \n" ++
    "_:c1 rel:stepson pers:St3 . \n" ++
    "_:c2 rel:stepson pers:Gr3 . \n" ++
    "_:c2 rel:son     pers:St3 . \n"

bwd24a2 :: RDFGraph
bwd24a2    = makeRDFGraphFromN3String bwd24a2str

bwd24a2str :: String
bwd24a2str = prefix2 ++
    "_:c1 rel:son     pers:Gr3 . \n" ++
    "_:c1 rel:stepson pers:St3 . \n"

bwd24a3 :: RDFGraph
bwd24a3    = makeRDFGraphFromN3String bwd24a3str

bwd24a3str :: String
bwd24a3str = prefix2 ++
    "_:c2 rel:stepson pers:Gr3 . \n" ++
    "_:c2 rel:son     pers:St3 . \n"

bwd24a4 :: RDFGraph
bwd24a4    = makeRDFGraphFromN3String bwd24a4str

bwd24a4str :: String
bwd24a4str = prefix2 ++
    "_:c1 rel:son     pers:Gr3 . \n" ++
    "_:c1 rel:stepson pers:St3 . \n" ++
    "_:c2 rel:stepson pers:Gr3 . \n" ++
    "_:c2 rel:son     pers:St3 . \n"

rul24 :: RDFRule
rul24 = makeN3ClosureSimpleRule scope2 "rul24" query24str result24str

fwd24 :: [RDFGraph]
fwd24 = fwdApply rul24 [graph24]

bwd24 :: [[RDFGraph]]
bwd24 = bwdApply rul24 result24a

--  bwd chain from partial conclusion
--  Also, fail because conclusion is more than the rule
--  can derive from any input.

query25 :: RDFGraph
query25    = makeRDFGraphFromN3String query25str

query25str :: String
query25str = prefix2 ++
    "?a rel:son      ?b . \n" ++
    "?a rel:daughter ?c . \n"

result25 :: RDFGraph
result25    = makeRDFGraphFromN3String result25str

result25str :: String
result25str = prefix2 ++
    "?b rel:sister  ?c . \n" ++
    "?c rel:brother ?b . \n"

result25a :: RDFGraph
result25a    = makeRDFGraphFromN3String result25astr

result25astr :: String
result25astr = prefix2 ++
    "pers:Wi3 rel:sister  pers:Ma3 . \n" ++
    "pers:Ma3 rel:brother pers:Wi3 . \n" ++
    "pers:Ro4 rel:sister  pers:Rh4 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n"

{-
result25b    = makeRDFGraphFromN3String result25bstr
result25bstr = prefix2 ++
    "pers:Ro4 rel:sister  pers:Rh4 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n"
-}

result25c :: RDFGraph
result25c    = makeRDFGraphFromN3String result25cstr

result25cstr :: String
result25cstr = prefix2 ++
    "pers:Wi3 rel:sister  pers:Ma3 . \n" ++
    "pers:Ma3 rel:brother pers:Wi3 . \n" ++
    "pers:Ro4 rel:sister  pers:Rh4 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n" ++
    "pers:xx3 rel:mother  pers:yy3 . \n" ++
    "pers:yy3 rel:brother pers:xx3 . \n"

result25d :: RDFGraph
result25d    = makeRDFGraphFromN3String result25dstr

result25dstr :: String
result25dstr = prefix2 ++
    "pers:Wi3 rel:sister  pers:Ma3 . \n" ++
    "pers:Ma3 rel:brother pers:Wi3 . \n" ++
    "pers:Ro4 rel:sister  pers:Rh4 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n" ++
    "pers:xx3 rel:father  pers:yy3 . \n"

conc25 :: RDFGraph
conc25    = makeRDFGraphFromN3String conc25str

conc25str :: String
conc25str = prefix2 ++
    "pers:Wi3 rel:sister  pers:Ma3 . \n" ++
    "pers:Rh4 rel:brother pers:Ro4 . \n"

bwd25a :: RDFGraph
bwd25a    = makeRDFGraphFromN3String bwd25astr

bwd25astr :: String
bwd25astr = prefix2 ++
    "_:a1 rel:son      pers:Wi3 . \n" ++
    "_:a1 rel:daughter pers:Ma3 . \n" ++
    "_:a2 rel:son      pers:Ro4 . \n" ++
    "_:a2 rel:daughter pers:Rh4 . \n"

rul25 :: RDFRule
rul25 = makeN3ClosureSimpleRule scope2 "rul25" query25str result25str

fwd25 :: [RDFGraph]
fwd25 = fwdApply rul25 [graph2]

bwd25, bwd25c, bwd25d :: [[RDFGraph]]
bwd25 = bwdApply rul25 conc25
bwd25c = bwdApply rul25 result25c
bwd25d = bwdApply rul25 result25d

test2 :: Test
test2 = 
  TestList
  [ testEq "testResult21" 1 (length fwd21)
  , testIn "testResult21a" result21a fwd21
  , testEq "testBwd21"  1 (length $ head bwd21)
  , testIn "testBwd21a" bwd21a (head bwd21)
  , testEq "testResult22" 1 (length fwd22)
  , testIn "testResult22a" result22a fwd22
  , testEq "testBwd22"  1 (length $ head bwd22)
  , testIn "testBwd22a" bwd22a (head bwd22)
  , testEq "testResult23" 1 (length fwd23)
  , testIn "testResult23a" result23a fwd23
  , testEq "testBwd23"  1 (length $ head bwd23)
  , testIn "testBwd23a" bwd23a (head bwd23)
  , testEq "testResult24" 1 (length fwd24)
  , testIn "testResult24a" result24a fwd24
  , testEq "testBwd24"  4 (length bwd24)
  , testIn "testBwd24a1" bwd24a1 (bwd24!!0)
  , testIn "testBwd24a2" bwd24a2 (bwd24!!1)
  , testIn "testBwd24a3" bwd24a3 (bwd24!!2)
  , testIn "testBwd24a4" bwd24a4 (bwd24!!3)
  , testEq "testResult25" 1 (length fwd25)
  , testIn "testResult25a" result25a fwd25
  , testEq "testBwd25"  1 (length $ head bwd25)
  , testIn "testBwd25a" bwd25a (head bwd25)
    -- testBwd25a1 = testEq "testBwd25a" bwd25a (head $ head bwd25)
  , testNo "testBwd25c" bwd25c
  , testNo "testBwd25d" bwd25d
  ]

--  test3:  check variable binding filters

scope3 :: Namespace
scope3 = Namespace "scope3"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope3"

query31 :: RDFGraph
query31    = makeRDFGraphFromN3String query31str

query31str :: String
query31str = prefix2 ++
    "?a rel:son ?b . \n" ++
    "?a rel:son ?c . \n"

modify31 :: RDFVarBindingModify
modify31 = makeVarFilterModify $ varFilterNE (Var "b") (Var "c")

result31 :: RDFGraph
result31    = makeRDFGraphFromN3String result31str

result31str :: String
result31str = prefix2 ++
    "?b rel:brother ?c . \n"

result31a :: RDFGraph
result31a    = makeRDFGraphFromN3String result31astr

result31astr :: String
result31astr = prefix2 ++
    "pers:Gr3 rel:brother pers:La3 . \n" ++
    "pers:Gr3 rel:brother pers:Si3 . \n" ++
    "pers:Gr3 rel:brother pers:Al3 . \n" ++
    "pers:La3 rel:brother pers:Gr3 . \n" ++
    "pers:La3 rel:brother pers:Si3 . \n" ++
    "pers:La3 rel:brother pers:Al3 . \n" ++
    "pers:Si3 rel:brother pers:Gr3 . \n" ++
    "pers:Si3 rel:brother pers:La3 . \n" ++
    "pers:Si3 rel:brother pers:Al3 . \n" ++
    "pers:Al3 rel:brother pers:Gr3 . \n" ++
    "pers:Al3 rel:brother pers:La3 . \n" ++
    "pers:Al3 rel:brother pers:Si3 . \n" ++
    "pers:Ol4 rel:brother pers:Lo4 . \n" ++
    "pers:Lo4 rel:brother pers:Ol4 . \n" ++
    "pers:Ha4 rel:brother pers:El4 . \n" ++
    "pers:El4 rel:brother pers:Ha4 . \n"

result31b :: RDFGraph
result31b    = makeRDFGraphFromN3String result31bstr

result31bstr :: String
result31bstr = prefix2 ++
    "pers:Gr3 rel:brother pers:Gr3 . \n"

result31c :: RDFGraph
result31c    = makeRDFGraphFromN3String result31cstr

result31cstr :: String
result31cstr = prefix2 ++
    "pers:Gr3 rel:brother pers:La3 . \n"

bwd31c :: RDFGraph
bwd31c    = makeRDFGraphFromN3String bwd31cstr

bwd31cstr :: String
bwd31cstr = prefix2 ++
    "_:a rel:son pers:Gr3 . \n" ++
    "_:a rel:son pers:La3 . \n"

rul31 :: RDFRule
rul31 = makeN3ClosureRule scope3 "rul31" query31str result31str modify31

fwd31 :: [RDFGraph]
fwd31 = fwdApply rul31 [graph2]

calcbwd31b, calcbwd31c :: [[RDFGraph]]
calcbwd31b = bwdApply rul31 result31b
calcbwd31c = bwdApply rul31 result31c

test3 :: Test
test3 = 
  TestList
  [ testEq "testResult31" 1 (length fwd31)
  , testIn "testResult31a" result31a fwd31
  , testEq "testBwd31"  0 (length calcbwd31b)
  , testEq "testBwd31"  1 (length $ head calcbwd31c)
  , testIn "testBwd31c" bwd31c (head calcbwd31c)
  ]

--  Instance entailment tests

scope4 :: Namespace
scope4 = Namespace "scope4"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope4"

graph4 :: RDFGraph
graph4    = makeRDFGraphFromN3String graph4str

graph4str :: String
graph4str = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter pers:Rh4 . \n"

vocab4 :: [RDFLabel]
vocab4 = allNodes (not . labelIsVar) graph4

name4 :: ScopedName
name4 = ScopedName scope4 "instance4"

rule4 :: RDFRule
rule4 = makeRdfInstanceEntailmentRule name4 vocab4

fwd42a :: RDFGraph
fwd42a    = makeRDFGraphFromN3String fwd42astr

fwd42astr :: String
fwd42astr = prefix2 ++
    "pers:Gr3 rel:son      _:Ro4 ;    \n" ++
    "         rel:daughter pers:Rh4 . \n"

fwd42b :: RDFGraph
fwd42b    = makeRDFGraphFromN3String fwd42bstr

fwd42bstr :: String
fwd42bstr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter _:Rh4 .    \n"

fwd42c :: RDFGraph
fwd42c    = makeRDFGraphFromN3String fwd42cstr

fwd42cstr :: String
fwd42cstr = prefix2 ++
    "pers:Gr3 rel:son      _:Ro4 ;    \n" ++
    "         rel:daughter _:Rh4 .    \n"

fwd42d :: RDFGraph
fwd42d    = makeRDFGraphFromN3String fwd42dstr

fwd42dstr :: String
fwd42dstr = prefix2 ++
    "_:Gr3    rel:son      _:Ro4 ;    \n" ++
    "         rel:daughter pers:Rh4 . \n"

fwd42e :: RDFGraph
fwd42e    = makeRDFGraphFromN3String fwd42estr

fwd42estr :: String
fwd42estr = prefix2 ++
    "_:Gr3    rel:son      _:Ro4 ;    \n" ++
    "         rel:daughter pers:Rh4 . \n"

fwd42f :: RDFGraph
fwd42f    = makeRDFGraphFromN3String fwd42fstr

fwd42fstr :: String
fwd42fstr = prefix2 ++
    "_:Gr3    rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter _:Rh4 .    \n"

fwd42g :: RDFGraph
fwd42g    = makeRDFGraphFromN3String fwd42gstr

fwd42gstr :: String
fwd42gstr = prefix2 ++
    "_:Gr3    rel:son      _:Ro4 ;    \n" ++
    "         rel:daughter _:Rh4 .    \n"

--  Non-entailments

fwd42w :: RDFGraph
fwd42w    = makeRDFGraphFromN3String fwd42wstr

fwd42wstr :: String
fwd42wstr = prefix2 ++
    "pers:Gr3 rel:daughter pers:Ro4 . \n"

fwd42x :: RDFGraph
fwd42x    = makeRDFGraphFromN3String fwd42xstr

fwd42xstr :: String
fwd42xstr = prefix2 ++
    "pers:Gr3 rel:daughter pers:Ro4 . \n"

fwd42y :: RDFGraph
fwd42y    = makeRDFGraphFromN3String fwd42ystr

fwd42ystr :: String
fwd42ystr = prefix2 ++
    "_:Gr3    rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter pers:Ro4 . \n"

fwd42z :: RDFGraph
fwd42z    = makeRDFGraphFromN3String fwd42zstr

fwd42zstr :: String
fwd42zstr = prefix2 ++
    "_:Gr3    rel:son      _:Ro4 ; \n" ++
    "         rel:son      _:Rh4 . \n"

bwd43 :: RDFGraph
bwd43 = makeRDFGraphFromN3String bwd43str

bwd43str :: String
bwd43str = prefix2 ++
    "_:a1 rel:son      pers:Ro4 . \n" ++
    "_:a2 rel:daughter pers:Rh4 . \n"

bwd43a :: RDFGraph
bwd43a = makeRDFGraphFromN3String bwd43astr

bwd43astr :: String
bwd43astr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n" ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n"

bwd43b :: RDFGraph
bwd43b = makeRDFGraphFromN3String bwd43bstr

bwd43bstr :: String
bwd43bstr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n" ++
    "pers:Ro4 rel:daughter pers:Rh4 . \n"

bwd43c :: RDFGraph
bwd43c = makeRDFGraphFromN3String bwd43cstr

bwd43cstr :: String
bwd43cstr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n" ++
    "pers:Rh4 rel:daughter pers:Rh4 . \n"

bwd43d :: RDFGraph
bwd43d = makeRDFGraphFromN3String bwd43dstr

bwd43dstr :: String
bwd43dstr = prefix2 ++
    "pers:Ro4 rel:son      pers:Ro4 . \n" ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n"

bwd43e :: RDFGraph
bwd43e = makeRDFGraphFromN3String bwd43estr

bwd43estr :: String
bwd43estr = prefix2 ++
    "pers:Ro4 rel:son      pers:Ro4 . \n" ++
    "pers:Ro4 rel:daughter pers:Rh4 . \n"

bwd43f :: RDFGraph
bwd43f = makeRDFGraphFromN3String bwd43fstr

bwd43fstr :: String
bwd43fstr = prefix2 ++
    "pers:Ro4 rel:son      pers:Ro4 . \n" ++
    "pers:Rh4 rel:daughter pers:Rh4 . \n"

bwd43g :: RDFGraph
bwd43g = makeRDFGraphFromN3String bwd43gstr

bwd43gstr :: String
bwd43gstr = prefix2 ++
    "pers:Rh4 rel:son      pers:Ro4 . \n" ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n"

bwd43h :: RDFGraph
bwd43h = makeRDFGraphFromN3String bwd43hstr

bwd43hstr :: String
bwd43hstr = prefix2 ++
    "pers:Rh4 rel:son      pers:Ro4 . \n" ++
    "pers:Ro4 rel:daughter pers:Rh4 . \n"

bwd43i :: RDFGraph
bwd43i = makeRDFGraphFromN3String bwd43istr

bwd43istr :: String
bwd43istr = prefix2 ++
    "pers:Rh4 rel:son      pers:Ro4 . \n" ++
    "pers:Rh4 rel:daughter pers:Rh4 . \n"

--  Forward chaining

fwdApply42 :: [RDFGraph]
fwdApply42      = fwdApply rule4 [graph4]

--  Backward chaining

bwdApply43 :: [[RDFGraph]]
bwdApply43      = bwdApply rule4 bwd43

test4 :: Test
test4 = 
  TestList
  [ 
    --  Check basics
    testEq "testRuleName41" name4 (ruleName rule4)
  , testEq "testVocab41"    3     (length vocab4)
  , testEq "testFwdLength42" 7 (length fwdApply42)
  , testIn "testFwdApply42a"  fwd42a fwdApply42
  , testIn "testFwdApply42b"  fwd42b fwdApply42
  , testIn "testFwdApply42c"  fwd42c fwdApply42
  , testIn "testFwdApply42d"  fwd42d fwdApply42
  , testIn "testFwdApply42e"  fwd42e fwdApply42
  , testIn "testFwdApply42f"  fwd42f fwdApply42
  , testIn "testFwdApply42g"  fwd42g fwdApply42
  , testEq "testBwdLength43" 9 (length bwdApply43)
  , testIn "testBwdApply43a"  [bwd43a] bwdApply43
  , testIn "testBwdApply43b"  [bwd43b] bwdApply43
  , testIn "testBwdApply43c"  [bwd43c] bwdApply43
  , testIn "testBwdApply43d"  [bwd43d] bwdApply43
  , testIn "testBwdApply43e"  [bwd43e] bwdApply43
  , testIn "testBwdApply43f"  [bwd43f] bwdApply43
  , testIn "testBwdApply43g"  [bwd43g] bwdApply43
  , testIn "testBwdApply43h"  [bwd43h] bwdApply43
  , testIn "testBwdApply43i"  [bwd43i] bwdApply43
      
    --  Entailment checks
  , testEq "testEntail44a" True  (checkInference rule4 [graph4] fwd42a)
  , testEq "testEntail44b" True  (checkInference rule4 [graph4] fwd42b)
  , testEq "testEntail44g" True  (checkInference rule4 [graph4] fwd42g)
  , testEq "testEntail44w" False (checkInference rule4 [graph4] fwd42w)
  , testEq "testEntail44x" False (checkInference rule4 [graph4] fwd42x)
  , testEq "testEntail44y" False (checkInference rule4 [graph4] fwd42y)
  , testEq "testEntail44z" False (checkInference rule4 [graph4] fwd42z)
  ]

--  Subgraph entailment tests

scope5 :: Namespace
scope5 = Namespace "scope5"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope5"

graph5 :: RDFGraph
graph5    = makeRDFGraphFromN3String graph5str

graph5str :: String
graph5str = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter pers:Rh4 . \n" ++
    "pers:Si3 rel:son      pers:Ol4 . \n"

name5 :: ScopedName
name5 = ScopedName scope5 "subgraph5"

rule5 :: RDFRule
rule5 = makeRdfSubgraphEntailmentRule name5

--  Forward chaining excludes null agraph and copy of antecedent

fwd52a :: RDFGraph
fwd52a    = makeRDFGraphFromN3String fwd52astr

fwd52astr :: String
fwd52astr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n"

fwd52b :: RDFGraph
fwd52b    = makeRDFGraphFromN3String fwd52bstr

fwd52bstr :: String
fwd52bstr = prefix2 ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n"

fwd52c :: RDFGraph
fwd52c    = makeRDFGraphFromN3String fwd52cstr

fwd52cstr :: String
fwd52cstr = prefix2 ++
    "pers:Si3 rel:son      pers:Ol4 . \n"

fwd52d :: RDFGraph
fwd52d    = makeRDFGraphFromN3String fwd52dstr

fwd52dstr :: String
fwd52dstr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n" ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n"

fwd52e :: RDFGraph
fwd52e    = makeRDFGraphFromN3String fwd52estr

fwd52estr :: String
fwd52estr = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 . \n" ++
    "pers:Si3 rel:son      pers:Ol4 . \n"

fwd52f :: RDFGraph
fwd52f    = makeRDFGraphFromN3String fwd52fstr

fwd52fstr :: String
fwd52fstr = prefix2 ++
    "pers:Gr3 rel:daughter pers:Rh4 . \n" ++
    "pers:Si3 rel:son      pers:Ol4 . \n"


--  Forward chaining

fwdApply52 :: [RDFGraph]
fwdApply52      = fwdApply rule5 [graph5]

test5 :: Test
test5 = 
  TestList
  [ testEq "testRuleName51" name5 (ruleName rule5)
  , testEq "testFwdLength52" 6 (length fwdApply52)
  , testIn "testFwdApply52a"  fwd52a fwdApply52
  , testIn "testFwdApply52b"  fwd52b fwdApply52
  , testIn "testFwdApply52c"  fwd52c fwdApply52
  , testIn "testFwdApply52d"  fwd52d fwdApply52
  , testIn "testFwdApply52e"  fwd52e fwdApply52
  , testIn "testFwdApply52f"  fwd52f fwdApply52
  ]

--  Simple entailment test
--  Simple entailment provides entailment check only, no forward or
--  backward chaining.  For that use instance- and subgraph- rules.

scope6 :: Namespace
scope6 = Namespace "scope6"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope6"

graph6 :: RDFGraph
graph6    = makeRDFGraphFromN3String graph6str

graph6str :: String
graph6str = prefix2 ++
    "pers:Gr3 rel:son      pers:Ro4 ; \n" ++
    "         rel:daughter pers:Rh4 . \n" ++
    "pers:Si3 rel:son      pers:Ol4 ; \n" ++
    "         rel:son      pers:Lo4 . \n"

name6 :: ScopedName
name6 = ScopedName scope5 "subgraph6"

rule6 :: RDFRule
rule6 = makeRdfSimpleEntailmentRule name6

simple6a :: RDFGraph
simple6a    = makeRDFGraphFromN3String simple6astr

simple6astr :: String
simple6astr = prefix2 ++
    "_:Gr3 rel:son      pers:Ro4 ; \n" ++
    "      rel:daughter pers:Rh4 . \n"

simple6b :: RDFGraph
simple6b    = makeRDFGraphFromN3String simple6bstr

simple6bstr :: String
simple6bstr = prefix2 ++
    "_:Si3 rel:son      pers:Ol4 ; \n" ++
    "      rel:son      pers:Lo4 . \n"

simple6c :: RDFGraph
simple6c    = makeRDFGraphFromN3String simple6cstr

simple6cstr :: String
simple6cstr = prefix2 ++
    "_:Si3 rel:son      _:Ol4 ; \n" ++
    "      rel:son      _:Lo4 . \n"

simple6d :: RDFGraph
simple6d    = makeRDFGraphFromN3String simple6dstr

simple6dstr :: String
simple6dstr = prefix2 ++
    "_:Si3 rel:son      _:Ol4 ; \n" ++
    "      rel:daughter _:Lo4 . \n"

simple6e :: RDFGraph
simple6e    = makeRDFGraphFromN3String simple6estr

simple6estr :: String
simple6estr = prefix2 ++
    "_:Si3 rel:daughter _:Ol4 ; \n" ++
    "      rel:mother   _:Lo4 . \n"

test6 :: Test
test6 = 
  TestList
  [ testEq "testRuleName61" name6 (ruleName rule6)
  , test "testSimple62" (checkInference rule6 [graph6] simple6a)
  , test "testSimple63" (checkInference rule6 [graph6] simple6b)
  , test "testSimple64" (checkInference rule6 [graph6] simple6c)
  , test "testSimple65" (checkInference rule6 [graph6] simple6d)
  , test "testSimple66" (not $ checkInference rule6 [graph6] simple6e)
  , test "testFwd64"    (null $ fwdApply rule6 [graph6])
  , test "testBwd65"    (null $ bwdApply rule6 graph6)
  ]

--  Test forward chaining node allocation logic
--
--  ?a uncle ?c => ?a father ?b, ?b brother ?c,   ?b allocTo ?a
--
--    Ro4 uncle La3, Ro4 uncle Si3, Rh4 uncle La3, Rh4 uncle Si3
--  =>
--    Ro4 father _:f1, _:f1 brother La3,
--    Ro4 father _:f1, _:f1 brother Si3,
--    Rh4 father _:f2, _:f2 brother La3,
--    Rh4 father _:f2, _:f2 brother Si3

scope7 :: Namespace
scope7 = Namespace "scope7"
    "http://id.ninebynine.org/wip/2003/rdfprooftest/scope7"

graph7 :: RDFGraph
graph7    = makeRDFGraphFromN3String graph7str

graph7str :: String
graph7str = prefix2 ++
    "pers:Ro4 rel:uncle pers:La3 ; \n" ++
    "         rel:uncle pers:Si3 . \n" ++
    "pers:Rh4 rel:uncle pers:La3 ; \n" ++
    "         rel:uncle pers:Si3 . \n"

query71 :: RDFGraph
query71    = makeRDFGraphFromN3String query71str

query71str :: String
query71str = prefix2 ++
    "?a rel:uncle ?c . \n"

result71 :: RDFGraph
result71    = makeRDFGraphFromN3String result71str

result71str :: String
result71str = prefix2 ++
    "?a rel:father  ?b . \n" ++
    "?b rel:brother ?c . \n"

result71a :: RDFGraph
result71a    = makeRDFGraphFromN3String result71astr

result71astr :: String
result71astr = prefix2 ++
    "pers:Ro4 rel:father  _:f1     . \n" ++
    "_:f1     rel:brother pers:La3 . \n" ++
    "pers:Ro4 rel:father  _:f1     . \n" ++
    "_:f1     rel:brother pers:Si3 . \n" ++
    "pers:Rh4 rel:father  _:f2     . \n" ++
    "_:f2     rel:brother pers:La3 . \n" ++
    "pers:Rh4 rel:father  _:f2     . \n" ++
    "_:f2     rel:brother pers:Si3 . \n"

rul71 :: RDFRule
rul71 = makeN3ClosureAllocatorRule scope7 "rul71"
    query71str result71str varBindingId mod71

mod71 :: [RDFLabel] -> RDFVarBindingModify
mod71 = makeNodeAllocTo (Var "b") (Var "a")

var71 :: [RDFVarBinding]
var71      = rdfQueryFind query71 graph7

var71a :: [VarBinding RDFLabel RDFLabel]
var71a     = vbmApply (mod71 (allLabels labelIsVar graph7)) var71

var71_1 :: VarBinding RDFLabel RDFLabel
var71_1    = head var71a

map71a, map71b, map71c :: Maybe RDFLabel
map71a     = Just (Var "#a")
map71b     = Just (Var "#b")
map71c     = Just (Var "#c")

sub71a :: [RDFGraph]
sub71a     = rdfQuerySubs var71a result71

fwd71 :: [RDFGraph]
fwd71 = fwdApply rul71 [graph7]

test7 :: Test
test7 = 
  TestList
  [ testEq "testVar71" 4 (length var71)
  , testEq "testVar71a" 4 (length var71a)
-- testVar71_1a = testEq "testVar71_1a" map71a ( vbMap var71_1 (Var "a"))
-- testVar71_1b = testEq "testVar71_1b" map71b ( vbMap var71_1 (Var "b"))
-- testVar71_1c = testEq "testVar71_1c" map71c ( vbMap var71_1 (Var "c"))
  , testEq "testVar71a" 4 (length sub71a)
  , testEq "testResult71" 1 (length fwd71)
  , testIn "testResult71a" result71a fwd71
  ]    

--  Full test suite, main program, and useful expressions for interactive use

allTests :: Test
allTests = TestList
  [ test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
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
