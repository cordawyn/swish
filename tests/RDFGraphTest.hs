{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFGraphTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module contains test cases for module RDFGraph.
--
--------------------------------------------------------------------------------

module Main where

import Swish.Utils.LookupMap
    ( LookupMap(..), LookupEntryClass(..)
    , mapFindMaybe )

import Swish.Utils.ListHelpers (equiv)

import Swish.RDF.GraphClass
    ( Label(..), Arc, arc )

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    , nullScopedName
    , makeUriScopedName
    )

import Swish.RDF.RDFGraph
    ( RDFTriple, 
      RDFGraph, 
      RDFLabel(..), ToRDFLabel(..), FromRDFLabel(..),
      NSGraph(..)
    , isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral
    , isDatatyped, isMemberProp
    , isUri, isBlank, isQueryVar, makeBlank
    , getScopedName
    , LookupFormula(..), FormulaMap, emptyFormulaMap
    , getArcs, addArc
    , remapLabels, remapLabelList
    , setFormulae, getFormulae, setFormula, getFormula
    , newNode, newNodes )

import Swish.RDF.Vocabulary
  ( namespaceRDF
  , langName 
  , rdf_XMLLiteral
  , xsd_boolean
  , xsd_integer
  , xsd_float
  , xsd_double
  , xsd_dateTime
  , xsd_date
    )

import qualified Data.Traversable as T

import Data.Monoid (Monoid(..))
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

import System.Locale (defaultTimeLocale)
import Data.Time (UTCTime(..), Day, fromGregorian, buildTime)

import Test.HUnit
    ( Test(TestCase,TestList,TestLabel)
    , Assertion
    , assertBool, assertEqual, assertString
    , runTestTT )

------------------------------------------------------------
--  Common definitions
------------------------------------------------------------

testCompare :: (Eq a, Show a) => String -> String -> a -> a -> Test
testCompare typ lab a1 a2 =
    TestCase ( assertEqual (typ++lab) a1 a2 )

testCompareEq :: (Eq a, Show a) => String -> String -> Bool -> a -> a -> Test
testCompareEq typ lab eq a1 a2 =
    TestCase ( assertEqual (typ++lab) eq (a1==a2) )

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq = testCompare "testEq"

------------------------------------------------------------
--  Test language tag comparisons
------------------------------------------------------------

type Lang = Maybe ScopedName

lt0, lt1, lt2, lt3, lt4, lt5, lt6,
  lt7, lt8 :: Lang
lt0 = Nothing
lt1 = Just (langName "en")
lt2 = Just (langName "EN")
lt3 = Just (langName "fr")
lt4 = Just (langName "FR")
lt5 = Just (langName "en-us")
lt6 = Just (langName "en-US")
lt7 = Just (langName "EN-us")
lt8 = Just (langName "EN-US")

langlist :: [(String, Lang)]
langlist =
  [ ("lt0",lt0),
    ("lt1",lt1), ("lt2",lt2), ("lt3",lt3), ("lt4",lt4),
    ("lt5",lt5), ("lt6",lt6), ("lt7",lt7), ("lt8",lt8) ]

langeqlist :: [(String, String)]
langeqlist =
  [
    ("lt1","lt2"),
    ("lt3","lt4"),
    ("lt5","lt6"),
    ("lt5","lt7"),
    ("lt5","lt8"),
    ("lt6","lt7"),
    ("lt6","lt8"),
    ("lt7","lt8")
  ]

testLangEq :: String -> Bool -> Lang -> Lang -> Test
testLangEq = testCompareEq "testLangEq:"

testLangEqSuite :: Test
testLangEqSuite = TestList
  [ testLangEq (tLab ll1 ll2) (tEq  ll1 ll2) t1 t2
      | (ll1,t1) <- langlist , (ll2,t2) <- langlist ]
    where
    tLab ll1 ll2 = ll1 ++ "-" ++ ll2
    tEq  ll1 ll2 = (ll1 == ll2)        ||
         (ll1,ll2) `elem` langeqlist ||
         (ll2,ll1) `elem` langeqlist

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

-- TODO: using a base of "" or "?" causes a fromJust failure somewhere
basee, baseu, base1, base2, base3, base4 :: Namespace
basee = Namespace ""      "http://example.com/a#"
baseu = Namespace "?"     "http://example.com/"
base1 = Namespace "base1" "http://id.ninebynine.org/wip/2003/test/graph1/node#"
base2 = Namespace "base2" "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3 = Namespace "base3" "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = Namespace "base4" "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"

qbes1, qbus1, qb1s1, qb2s2, qb3s3, qb3, qb3bm, qb4m :: ScopedName
qbes1 = ScopedName basee "s1"
qbus1 = ScopedName baseu "s1"
qb1s1 = ScopedName base1 "s1"
qb2s2 = ScopedName base2 "s2"
qb3s3 = ScopedName base3 "s3"
qb3   = ScopedName base3 ""
qb3bm = ScopedName base3 "basemore"
qb4m  = ScopedName base4 "more"

es1, us1, s1, s2, s3, s4, s5, s6, s7, s8 :: RDFLabel
es1 = Res qbes1
us1 = Res qbus1
s1 = toRDFLabel qb1s1 
s2 = toRDFLabel qb2s2 
s3 = toRDFLabel qb3s3 
s4 = Res qb3   
s5 = Blank "s5"
s6 = Res qb3bm 
s7 = Res qb4m  
s8 = Blank "s8"

st1, st2, st3 :: RDFLabel
st1 = toRDFLabel $ ScopedName base1 "st1"
st2 = toRDFLabel $ ScopedName base2 "st2" 
st3 = toRDFLabel $ ScopedName base3 "st3"

bb, bb0, b1, b2, b3, b4, b5, b6, b7,
  b8, b9, b10 :: RDFLabel
bb  = Blank "bb" 
bb0 = Blank "bb0"
b1  = Blank "b1" 
b2  = Blank "b2" 
b3  = Blank "b3" 
b4  = Blank "b4" 
b5  = Blank "b5" 
b6  = Blank "b6" 
b7  = Blank "b7" 
b8  = Blank "b8" 
b9  = Blank "b9" 
b10 = Blank "b10"

c1, c2, c3, c4 :: RDFLabel
c1 = Blank "c1"
c2 = Blank "c2"
c3 = Blank "c3"
c4 = Blank "c4"

ba1, ba2, ba3, ba4 :: RDFLabel
ba1 = Blank "_1"
ba2 = Blank "_2"
ba3 = Blank "_3"
ba4 = Blank "_4"

bn3, bn4, bn5, bn6 :: RDFLabel
bn3 = Blank "3"
bn4 = Blank "4"
bn5 = Blank "5"
bn6 = Blank "6"

p1, p2, p3, p4 :: RDFLabel
p1 = Res $ ScopedName base1 "p1"
p2 = Res $ ScopedName base2 "p2"
p3 = Res $ ScopedName base3 "p3"
p4 = Res $ ScopedName base3 "p4"

o1, o2, o3, o4, o5, o6 :: RDFLabel
o1 = Res $ ScopedName base1 "o1"
o2 = Res $ ScopedName base2 "o2"
o3 = Res $ ScopedName base3 "o3"
o4 = toRDFLabel qb3   
o5 = Blank "o5"
o6 = Blank "s5"

qb1t1, qb1t2 :: ScopedName
qb1t1 = ScopedName base1 "type1"
qb1t2 = ScopedName base1 "type2"

l1, l2, l2gb, l3, l4, l5, l6, l7, l8,
  l9, l10, l11, l12 :: RDFLabel
l1   = "l1" -- use IsString instance
l2   = Lit "l2"  (Just (langName "en")) 
l2gb = Lit "l2"  (Just (langName "en-gb")) 
l3   = Lit "l2"  (Just (langName "fr")) 
l4   = Lit "l4"  (Just qb1t1)           
l5   = Lit "l4"  (Just qb1t1)           
l6   = Lit "l4"  (Just qb1t1)           
l7   = Lit "l4"  (Just qb1t2)           
l8   = Lit "l4"  (Just qb1t2)           
l9   = Lit "l4"  (Just qb1t2)           
l10  = Lit "l10" (Just rdf_XMLLiteral)  
l11  = Lit "l10" (Just rdf_XMLLiteral)  
l12  = Lit "l10" (Just rdf_XMLLiteral)  

v1, v2, v3, v4, vb3, vb4 :: RDFLabel
v1  = Var "v1"  
v2  = Var "v2"  
v3  = Var "v3"  
v4  = Var "v4"  
vb3 = Blank "v3"
vb4 = Blank "v4"

-- Test cases for isMemberProp

cm1, cm2, nm1, nm2 :: RDFLabel
cm1  = Res $ ScopedName namespaceRDF "_1"
cm2  = Res $ ScopedName namespaceRDF "_234567"
nm1  = Res $ ScopedName namespaceRDF "987"
nm2  = Res $ ScopedName namespaceRDF "_987a65"

------------------------------------------------------------
--  RDFLabel construction and equality tests
------------------------------------------------------------

testLabelEq :: String -> Bool -> RDFLabel -> RDFLabel -> Test
testLabelEq = testCompareEq "testLabelEq:"

nodelist :: [(String, RDFLabel)]
nodelist =
  [ ("es1",es1), ("us1",us1)
  , ("s1",s1), ("s2",s2), ("s3",s3), ("s4",s4), ("s5",s5)
  , ("s6",s6), ("s7",s7), ("s8",s8)
  , ("b1",b1), ("b2",b2), ("b3",b3), ("b4",b4)
  , ("p1",p1), ("p2",p2), ("p3",p3), ("p4",p4)
  , ("o1",o1), ("o2",o2), ("o3",o3), ("o4",o4), ("o5",o5)
  , ("l1",l1), ("l2",l2), ("l2gb",l2gb), ("l3",l3)
  , ("l4",l4), ("l5",l5), ("l6",l6)
  , ("l7",l7), ("l8",l8), ("l9",l9)
  , ("l10",l10), ("l11",l11), ("l12",l12)
  , ("v1",v1), ("v2",v2)
  ]

nodeeqlist :: [(String, String)]
nodeeqlist =
  [ ("s4","o4")
  , ("s5","o6")
  , ("s6","s7")
  , ("l4","l5")
  , ("l4","l6")
  , ("l5","l6")
  , ("l7","l8")
  , ("l7","l9")
  , ("l8","l9")
  , ("l10","l11")
  , ("l10","l12")
  , ("l11","l12")
  ]

testNodeEqSuite :: Test
testNodeEqSuite = TestList
  [ testLabelEq (tLab ll1 ll2) (tEq  ll1 ll2) n1 n2
      | (ll1,n1) <- nodelist , (ll2,n2) <- nodelist ]
    where
    tLab ll1 ll2 = ll1 ++ "-" ++ ll2
    tEq  ll1 ll2 = (ll1 == ll2)        ||
         (ll1,ll2) `elem` nodeeqlist ||
         (ll2,ll1) `elem` nodeeqlist

-- test ToRDFLabel/FromRDFlabel/IsString instances
--
    
testConv :: (ToRDFLabel a, FromRDFLabel a, Eq a, Show a) 
            => String -> String -> Maybe ScopedName -> a -> Test    
testConv lbl sVal dtype hVal = 
  let rdfVal = Lit sVal dtype
  in TestList
  [
    testEq ("tconv:" ++ lbl) rdfVal       (toRDFLabel hVal)
  , testEq ("fconv:" ++ lbl) (Just hVal)  (fromRDFLabel rdfVal)
  ]
    
testConversionSuite :: Test
testConversionSuite =
  TestList
  [
    -- failure case
    testEq "fconv:fail chr1"    (Nothing :: Maybe Char) (fromRDFLabel l1)
  , testEq "fconv:fail chr2"    (Nothing :: Maybe Char) (fromRDFLabel s1)
  , testEq "fconv:fail str1"    (Nothing :: Maybe String) (fromRDFLabel (Lit "1.23" (Just xsd_float)))
  , testEq "fconv:fail int1"    (Nothing :: Maybe Int)  (fromRDFLabel l1)
  , testEq "fconv:fail int2"    (Nothing :: Maybe Int)  (fromRDFLabel (Lit "123456789012345" (Just xsd_integer))) 
  , testEq "fconv:fail float1"  (Nothing :: Maybe Float)  (fromRDFLabel l1)
  , testEq "fconv:fail float2"  (Nothing :: Maybe Float)  (fromRDFLabel (Lit "1.234e101" (Just xsd_float))) -- invalid input 
  , testEq "fconv:fail float3"  (Nothing :: Maybe Float)  (fromRDFLabel (Lit "-1.234e101" (Just xsd_float))) -- invalid input 
  , testEq "fconv:fail float4"  (Nothing :: Maybe Float)  (fromRDFLabel (Lit "NaNs" (Just xsd_float))) -- invalid input 
  , testEq "fconv:fail dbl1"    (Nothing :: Maybe Double)  (fromRDFLabel (Lit "1.23" (Just xsd_float))) -- invalid input 
  , testEq "fconv: fail sn1"    (Nothing :: Maybe ScopedName) (fromRDFLabel l1)
    
    -- basic string tests
  , testEq "tconv:emptystring1"  (Lit "" Nothing)    ""       -- want to try out IsString so do not use testConv
  , testConv "emptystring2"       "" Nothing    (""::String)
  , testConv "char"                "x" Nothing   'x'
  , testEq "tconv:l1-1"          (Lit "l1" Nothing)  l1
  , testConv "l1-2"          "l1" Nothing  ("l1"::String)
    
    -- boolean
  , testConv "True"     "True"    (Just xsd_boolean) True
  , testConv "False"    "False"   (Just xsd_boolean) False
    
    -- numeric types
  , testConv "int 0"    "0"       (Just xsd_integer) (0::Int)
  , testConv "int -10"  "-10"     (Just xsd_integer) ((-10)::Int)
  , testConv "int 10"   "10"      (Just xsd_integer) (10::Int)
  , testConv "integer 0"    "0"       (Just xsd_integer) (0::Integer)
  , testConv "integer -10"  "-10"     (Just xsd_integer) ((-10)::Integer)
  , testConv "integer 10"   "10"      (Just xsd_integer) (10::Integer)
  , testConv "integer big"  "123456789012345678901234567890"      (Just xsd_integer) (123456789012345678901234567890::Integer)
  , testConv "integer -big" "-123456789012345678901234567890"     (Just xsd_integer) ((-123456789012345678901234567890)::Integer)
  , testConv "float 0"        "0.0"     (Just xsd_float) (0::Float)
  , testConv "float 0.2"      "0.2"     (Just xsd_float) (0.2::Float)
  , testConv "float -0.2"     "-0.2"    (Just xsd_float) ((-0.2)::Float)
  , testConv "float 2.01e-4"  "2.01e-4"  (Just xsd_float) (0.000201::Float)
  , testConv "float -2.01e-4" "-2.01e-4" (Just xsd_float) ((-0.000201)::Float)
  , testConv "float 2.01e38"  "2.01e38"  (Just xsd_float) (2.01e38::Float)
  , testConv "float -2.01e38" "-2.01e38" (Just xsd_float) ((-2.01e38)::Float)
  , testConv "double 0"        "0.0"     (Just xsd_double) (0::Double)
  , testConv "double 0.2"      "0.2"     (Just xsd_double) (0.2::Double)
  , testConv "double -0.2"     "-0.2"    (Just xsd_double) ((-0.2)::Double)
  , testConv "double 2.01e-4"  "2.01e-4"  (Just xsd_double) (0.000201::Double)
  , testConv "double -2.01e-4" "-2.01e-4" (Just xsd_double) ((-0.000201)::Double)
  , testConv "double 2.01e38"  "2.01e38"  (Just xsd_double) (2.01e38::Double)
  , testConv "double -2.01e38" "-2.01e38" (Just xsd_double) ((-2.01e38)::Double)
  , testConv "double 2.01e108"  "2.01e108"  (Just xsd_double) (2.01e108::Double)
  , testConv "double -2.01e108" "-2.01e108" (Just xsd_double) ((-2.01e108)::Double)
  
    -- URI related types
  , testEq "tconv:sname s1"    s1             (toRDFLabel qb1s1)
  , testEq "fconv:sname s1"    (Just qb1s1)   (fromRDFLabel s1)
    
    -- time values
  , testConv "time1"   "1970-01-01T00:00:00Z"            (Just xsd_dateTime)  utc1
  , testEq   "tconv:time2"   (Lit "2011-02-28T20:04:02.304Z" (Just xsd_dateTime))  (toRDFLabel utc2)
  , testEq   "fconv:time2a"  (Just utc2)                                           (fromRDFLabel (Lit "2011-02-28T20:04:02.304Z" (Just xsd_dateTime)))
  , testEq   "fconv:time2b"  (Just utc2)                                           (fromRDFLabel (Lit "2011-02-28T17:04:02.304-03:00" (Just xsd_dateTime)))
  , testEq   "fconv:time2c"  (Just utc2)                                           (fromRDFLabel (Lit "2011-03-01T00:04:02.304+04:00" (Just xsd_dateTime)))
  , testEq   "fconv:time2d"  (Just utc2)                                           (fromRDFLabel (Lit "2011-02-28T20:04:02.304" (Just xsd_dateTime)))
  , testConv "time2Z"  "2011-02-28T20:04:02.304Z"        (Just xsd_dateTime)  utc2
                              
  , testConv "day1a"   "1970-01-01Z"      (Just xsd_date) day1
  , testEq   "fconv:day1b"  (Just day1)   (fromRDFLabel (Lit "1970-01-01" (Just xsd_date)))
  , testEq   "fconv:day1c"  (Just day1)   (fromRDFLabel (Lit "1970-01-01-03:00" (Just xsd_date)))
  , testEq   "fconv:day1d"  (Just day1)   (fromRDFLabel (Lit "1970-01-01+04:00" (Just xsd_date)))
    
    -- TODO
    
  ]
  
utc1, utc2 :: UTCTime
utc1 = buildTime defaultTimeLocale []
utc2 =
  let dNum = fromGregorian 2011 2 28
      tDiff = (23.0 - 3.0) * 3600.0 + 4.0 * 60.0 + 2.304 
  in UTCTime dNum tDiff
     
day1 :: Day
day1 = fromGregorian 1970 1 1

------------------------------------------------------------
--  RDFLabel classification tests
------------------------------------------------------------

testClass :: String -> (RDFLabel -> Bool) -> RDFLabel -> Bool -> Test
testClass lab clsf nod eq = testCompare "testClass:" lab eq (clsf nod)

altIsXmlLit :: RDFLabel -> Bool
altIsXmlLit = isDatatyped rdf_XMLLiteral

testNodeClassSuite :: Test
testNodeClassSuite = TestList
  [ testClass "testClass01" isUri            s1  True
  , testClass "testClass02" isUri            s5  False
  , testClass "testClass03" isUri            ba1 False
  , testClass "testClass04" isUri            l1  False
  , testClass "testClass05" isUri            l10 False
  , testClass "testClass06" isUri            cm1 True
  , testClass "testClass07" isUri            nm1 True
  , testClass "testClass08" isUri            v1  False

  , testClass "testClass10" isLiteral        s1  False
  , testClass "testClass11" isLiteral        s5  False
  , testClass "testClass12" isLiteral        ba1 False
  , testClass "testClass13" isLiteral        l1  True
  , testClass "testClass14" isLiteral        l4  True
  , testClass "testClass15" isLiteral        l5  True
  , testClass "testClass16" isLiteral        l10 True
  , testClass "testClass17" isLiteral        l11 True
  , testClass "testClass18" isLiteral        cm1 False
  , testClass "testClass19" isLiteral        v1  False

  , testClass "testClass20" isTypedLiteral   s1  False
  , testClass "testClass21" isTypedLiteral   s5  False
  , testClass "testClass22" isTypedLiteral   ba1 False
  , testClass "testClass23" isTypedLiteral   l1  False
  , testClass "testClass24" isTypedLiteral   l2  False
  , testClass "testClass25" isTypedLiteral   l4  True
  , testClass "testClass26" isTypedLiteral   l5  True
  , testClass "testClass27" isTypedLiteral   l10 True
  , testClass "testClass28" isTypedLiteral   l11 True
  , testClass "testClass29" isTypedLiteral   v1  False

  , testClass "testClass30" isUntypedLiteral s1  False
  , testClass "testClass31" isUntypedLiteral s5  False
  , testClass "testClass32" isUntypedLiteral ba1 False
  , testClass "testClass33" isUntypedLiteral l1  True
  , testClass "testClass34" isUntypedLiteral l2  True
  , testClass "testClass35" isUntypedLiteral l4  False
  , testClass "testClass36" isUntypedLiteral l5  False
  , testClass "testClass37" isUntypedLiteral l10 False
  , testClass "testClass38" isUntypedLiteral l11 False
  , testClass "testClass39" isUntypedLiteral v1  False

  , testClass "testClass40" isXMLLiteral     s1  False
  , testClass "testClass41" isXMLLiteral     s5  False
  , testClass "testClass42" isXMLLiteral     ba1 False
  , testClass "testClass43" isXMLLiteral     l1  False
  , testClass "testClass44" isXMLLiteral     l2  False
  , testClass "testClass45" isXMLLiteral     l4  False
  , testClass "testClass46" isXMLLiteral     l5  False
  , testClass "testClass47" isXMLLiteral     l10 True
  , testClass "testClass48" isXMLLiteral     l11 True
  , testClass "testClass49" isXMLLiteral     v1  False

  , testClass "testClass50" altIsXmlLit      s1  False
  , testClass "testClass51" altIsXmlLit      s5  False
  , testClass "testClass52" altIsXmlLit      ba1 False
  , testClass "testClass53" altIsXmlLit      l1  False
  , testClass "testClass54" altIsXmlLit      l2  False
  , testClass "testClass55" altIsXmlLit      l4  False
  , testClass "testClass56" altIsXmlLit      l5  False
  , testClass "testClass57" altIsXmlLit      l10 True
  , testClass "testClass58" altIsXmlLit      l11 True

  , testClass "testClass60" isMemberProp     s1  False
  , testClass "testClass61" isMemberProp     s5  False
  , testClass "testClass62" isMemberProp     ba1 False
  , testClass "testClass63" isMemberProp     l1  False
  , testClass "testClass64" isMemberProp     l10 False
  , testClass "testClass65" isMemberProp     cm1 True
  , testClass "testClass66" isMemberProp     cm2 True
  , testClass "testClass67" isMemberProp     nm1 False
  , testClass "testClass68" isMemberProp     nm2 False

  , testClass "testClass70" isBlank          s7  False
  , testClass "testClass71" isBlank          s5  True
  , testClass "testClass72" isBlank          ba1 True
  , testClass "testClass73" isBlank          l1  False
  , testClass "testClass74" isBlank          l4  False
  , testClass "testClass75" isBlank          l5  False
  , testClass "testClass76" isBlank          l10 False
  , testClass "testClass77" isBlank          l11 False
  , testClass "testClass78" isBlank          cm1 False
  , testClass "testClass79" isBlank          v1  False

  , testClass "testClass80" isQueryVar       s8  False
  , testClass "testClass81" isQueryVar       s5  False
  , testClass "testClass82" isQueryVar       ba1 False
  , testClass "testClass83" isQueryVar       l1  False
  , testClass "testClass84" isQueryVar       l4  False
  , testClass "testClass85" isQueryVar       l5  False
  , testClass "testClass86" isQueryVar       l10 False
  , testClass "testClass87" isQueryVar       l11 False
  , testClass "testClass88" isQueryVar       cm1 False
  , testClass "testClass89" isQueryVar       v1  True
    
  ]

------------------------------------------------------------
--  RDFLabel local part separation and recombination tests
------------------------------------------------------------

testLocalEq :: String -> String -> String -> Test
testLocalEq = testCompare "testLocalEq:"

testLocalLabEq :: String -> RDFLabel -> RDFLabel -> Test
testLocalLabEq = testCompare "testLocalEq:"

testNodeLocalSuite :: Test
testNodeLocalSuite = TestList
  [ testLocalEq    "01" "b1"  (getLocal b1)
  , testLocalEq    "02" "b2"  (getLocal b2)
  , testLocalEq    "03" "?v1" (getLocal v1)
  , testLocalEq    "04" "?v2" (getLocal v2)
  , testLocalLabEq "05" b1    (makeLabel "b1")
  , testLocalLabEq "06" b2    (makeLabel "b2")
  , testLocalLabEq "07" v1    (makeLabel "?v1")
  , testLocalLabEq "08" v2    (makeLabel "?v2")
  ] 
  
------------------------------------------------------------
--  Node generation tests
------------------------------------------------------------

testNodeEq :: String -> RDFLabel -> RDFLabel -> Test
testNodeEq = testCompare "testNodeEq:"

tnn01, tnn02, tnn03, tnn04, tnn05, tnn06,
  tnn07, tnn08, tnn09 :: RDFLabel
tnn01 = (newNode  v1 [b1,b3,v1,v2])
tnn02 = (newNode  b1 [b1,b3,v1,v2])
tnn03 = (newNodes b1 [b1,b3,v1,v2])!!0
tnn04 = (newNodes b1 [b1,b3,v1,v2])!!1
tnn05 = (newNodes b1 [b1,b3,v1,v2])!!2
tnn06 = (newNodes s1 [b1,b3,v1,v2,tnns3])!!0
tnn07 = (newNodes s1 [b1,b3,v1,v2,tnns3])!!1
tnn08 = (newNodes s1 [b1,b3,v1,v2,tnns3])!!2
tnn09 = (newNodes l1 [b1,b3,v1,v2,tnns3])!!2

tnns1, tnns2, tnns3, tnns4, tnnl1 :: RDFLabel
tnns1 = Blank "Res_s1"
tnns2 = Blank "Res_s2"
tnns3 = Blank "Res_s3"
tnns4 = Blank "Res_s4"
tnnl1 = Blank "Lit_2"

testNewNodeSuite :: Test
testNewNodeSuite = TestList
  [ testNodeEq "testNewNode01" v3    tnn01
  , testNodeEq "testNewNode02" b2    tnn02
  , testNodeEq "testNewNode03" b2    tnn03
  , testNodeEq "testNewNode04" b4    tnn04
  , testNodeEq "testNewNode05" b5    tnn05
  , testNodeEq "testNewNode06" tnns1 tnn06
  , testNodeEq "testNewNode07" tnns2 tnn07
  , testNodeEq "testNewNode08" tnns4 tnn08
  , testNodeEq "testNewNode09" tnnl1 tnn09
  ]

------------------------------------------------------------
--  RDFLabel ordering tests
------------------------------------------------------------

testLabelOrd :: String -> Ordering -> RDFLabel -> RDFLabel -> Test
testLabelOrd lab order n1 n2 =
    TestCase ( assertEqual
               ("testLabelOrd:"++lab++"["++(show n1)++","++(show n2)++"]")
               order (compare n1 n2) )

nodeorder :: [String]
nodeorder =
  [ 
    -- literals
    "l1"
  , "l11", "l12", "l10"
  , "l2", "l2gb", "l3"
  , "l5", "l6", "l4", "l8", "l9", "l7"
  -- URIs beginning with ':' and '<'  
  , "es1"
  , "us1"
  -- variables
  , "v1", "v2"
  -- URIs
  , "o1", "p1", "s1"
  , "o2", "p2", "s2"
  , "s4", "o4", "s6", "s7"
  , "o3", "p3", "p4", "s3"
  -- blank nodes
  , "b1", "b2", "b3", "b4"
  , "o5", "s5", "s8"
  ]

testNodeOrdSuite :: Test
testNodeOrdSuite = TestList
  [ testLabelOrd (tLab ll1 ll2) (tOrd ll1 ll2) n1 n2
      | (ll1,n1) <- nodelist , (ll2,n2) <- nodelist ]
    where
    tLab ll1 ll2 = ll1 ++ "-" ++ ll2
    tOrd ll1 ll2
      | tEq ll1 ll2  = EQ
      | otherwise    = compare (fromJust $ elemIndex ll1 nodeorder)
                               (fromJust $ elemIndex ll2 nodeorder)
    tEq  ll1 ll2 = (ll1 == ll2)        ||
           (ll1,ll2) `elem` nodeeqlist ||
           (ll2,ll1) `elem` nodeeqlist

------------------------------------------------------------
--  Other RDFLabel tests
------------------------------------------------------------

testLabelOtherSuite :: Test
testLabelOtherSuite = TestList
    [ testEq "testLabelName01" (getScopedName s1) qb1s1
    , testEq "testLabelName02" (getScopedName b1) nullScopedName
    , testEq "testLabelName03" (getScopedName l1) nullScopedName
    , testEq "testLabelName04" (getScopedName v1) nullScopedName
    ]

------------------------------------------------------------
--  Statement construction and equality tests
------------------------------------------------------------

testStmtEq :: String -> Bool -> RDFTriple -> RDFTriple -> Test
testStmtEq = testCompareEq "testStmtEq:"

slist, plist, olist :: [(String, RDFLabel)]
slist =
  [
    ("s1",s1), ("s4",s4), ("s5",s5), ("s6",s6), ("s7",s7)
  ]

plist =
  [
    ("p1",p1)
  ]

olist =
  [ ("o1",o1), ("o4",o4), ("o5",o5),
    ("l1",l1), ("l4",l4), ("l7",l7), ("l8",l8), ("l10",l10)
  ]

tlist :: [(String, Arc RDFLabel)]
tlist =
  [ (lab s p o,trp s p o) | s <- slist, p <- plist, o <- olist ]
    where
    lab (s,_) (p,_) (o,_) = s++"."++p++"."++o
    trp (_,s) (_,p) (_,o) = arc s p o

stmteqlist :: [(String, String)]
stmteqlist =
  [
    ("s6.p1.l1", "s7.p1.l1"),
    ("s6.p1.l4", "s7.p1.l4"),
    ("s6.p1.l7", "s7.p1.l7"),
    ("s6.p1.l7", "s7.p1.l8"),
    ("s6.p1.l8", "s7.p1.l7"),
    ("s6.p1.l8", "s7.p1.l8"),
    ("s6.p1.l10","s7.p1.l10"),
    ("s6.p1.o1", "s7.p1.o1"),
    ("s6.p1.o4", "s7.p1.o4"),
    ("s6.p1.o5", "s7.p1.o5"),
    ("s1.p1.l7", "s1.p1.l8"),
    ("s4.p1.l7", "s4.p1.l8"),
    ("s5.p1.l7", "s5.p1.l8"),
    ("s6.p1.l7", "s6.p1.l8"),
    ("s7.p1.l7", "s7.p1.l8")
  ]

testStmtEqSuite :: Test
testStmtEqSuite = TestList
  [ testStmtEq (tLab ll1 ll2) (tEq  ll1 ll2) t1 t2
      | (ll1,t1) <- tlist , (ll2,t2) <- tlist ]
    where
    tLab ll1 ll2 = ll1 ++ "-" ++ ll2
    tEq  ll1 ll2 = (ll1 == ll2)        ||
            (ll1,ll2) `elem` stmteqlist ||
            (ll2,ll1) `elem` stmteqlist

------------------------------------------------------------
--  Graph construction and equality tests
------------------------------------------------------------

testGraphEq :: String -> Bool -> RDFGraph -> RDFGraph -> Test
testGraphEq lab eq gg1 gg2 =
    --  Set test False to get extra trace info about graph differences
    --  Some tests will fail with this setting, so revert to True to
    --  get test result.
    if True then
        testCompareEq "testGraphEq:" lab eq gg1 gg2
    else
        TestList
            [ TestCase ( assertEqual ("testGraphEq:"++lab) eq (gg1==gg2) )
            , TestCase ( assertEqual ("testGraphEq:"++lab) gg1 gg2 )
            ]

testGraphEqM :: String -> Bool -> Maybe RDFGraph -> Maybe RDFGraph -> Test
testGraphEqM = testCompareEq "testGraphEq:"

t01, t02, t03, t04, t05, t06 :: Arc RDFLabel
t01 = arc s1 p1 o1
t02 = arc s2 p1 o2
t03 = arc s3 p1 o3
t04 = arc s1 p1 l1
t05 = arc s2 p1 l4
t06 = arc s3 p1 l10

t10, t11, t12 :: Arc RDFLabel
t10 = arc s1 p1 b1
t11 = arc b1 p2 b2
t12 = arc b2 p3 o1

t20, t21, t22 :: Arc RDFLabel
t20 = arc s1 p1 b3
t21 = arc b3 p2 b4
t22 = arc b4 p3 o1

tt01, tt02, tt03, tt04, tt05, tt06 :: Arc RDFLabel
tt01 = arc st1 p1 o1
tt02 = arc st2 p1 o2
tt03 = arc st3 p1 o3
tt04 = arc st1 p1 l1
tt05 = arc st2 p1 l4
tt06 = arc st3 p1 l10

makeNewPrefixNamespace :: (String,Namespace) -> Namespace
makeNewPrefixNamespace (pre,ns) = Namespace pre (nsURI ns)

nslist :: LookupMap Namespace
nslist = LookupMap $ map makeNewPrefixNamespace
    [ ("base1",base1)
    , ("base2",base2)
    , ("base3",base3)
    , ("base4",base4)
    ]

nslistalt :: LookupMap Namespace
nslistalt = LookupMap $ map makeNewPrefixNamespace
    [ ("altbase1",base1)
    , ("altbase2",base2)
    , ("altbase3",base3)
    ]

toGraph :: [Arc RDFLabel] -> RDFGraph
toGraph stmts = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = stmts
        }

g1, gt1 :: RDFGraph
g1  = toGraph [t01]
gt1 = toGraph [tt01]

-- Check for nonsensitivety of graph equility to namespace differences:

g1alt :: RDFGraph
g1alt = NSGraph
        { namespaces = nslistalt
        , formulae   = emptyFormulaMap
        , statements = [t01]
        }

--  Construct version of g1 using just URIs

uris1, urip1, urio1 :: ScopedName
uris1 = makeUriScopedName "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"
urip1 = makeUriScopedName "http://id.ninebynine.org/wip/2003/test/graph1/node#p1"
urio1 = makeUriScopedName "http://id.ninebynine.org/wip/2003/test/graph1/node#o1"

tu01 :: Arc RDFLabel
tu01  = arc (Res uris1) (Res urip1) (Res urio1)

g2arcs :: [String]
g2arcs = [
  "(base1:s1,base1:p1,base1:o1)", 
  "(base2:s2,base1:p1,base2:o2)", 
  "(base3:s3,base1:p1,base3:o3)", 
  "(base1:s1,base1:p1,\"l1\")", 
  "(base2:s2,base1:p1,\"l4\"^^base1:type1)", 
  "(base3:s3,base1:p1,\"l10\"^^rdf:XMLLiteral)"
  ]
  
g2str :: String -> String
g2str sp = 
  let spaces = "    "
  in intercalate ('\n':sp) $ 
     ["Graph, formulae: ",
      "arcs: "]
     ++ map (spaces++) g2arcs
     
g1uri, g2, gt2, g3, gt3, g4, g5, g6, g7, g8, g9, g10 :: RDFGraph
g1uri = toGraph [tu01]
g2    = toGraph [t01,t02,t03,t04,t05,t06]
gt2   = toGraph [tt01,tt02,tt03,tt04,tt05,tt06]
g3    = toGraph [t06,t05,t04,t03,t02,t01]
gt3   = toGraph [tt06,tt05,tt04,tt03,tt02,tt01]
g4    = toGraph [t01,t02,t03,t04,t05,t06,t10,t11,t12]
g5    = toGraph [t01,t02,t03,t04,t05,t06,t20,t21,t22]
g6    = toGraph [t01,t02,t03,t04,t05,t06,t10,t11,t12,t20,t21,t22]
g7    = toGraph [t01,t02]
g8    = toGraph [t02,t01]
g9    = toGraph [t03,t02,t01]
g10   = toGraph [t02,t02,t01]

g9a, g10a :: RDFGraph
g9a  = addArc t03 g8
g10a = addArc t02 g8

glist :: [(String, RDFGraph)]
glist =
  [ ("g1",g1), ("g1alt",g1alt), ("g1uri",g1uri)
  , ("g2",g2), ("g3",g3), ("g4",g4), ("g5",g5), ("g6",g6)
  , ("g7",g7), ("g8",g8), ("g9",g9), ("g10",g10)
  , ("g9a",g9a), ("g10a",g10a)
  ]

grapheqlist :: [(String, String)]
grapheqlist =
  [ ("g1","g1alt")
  , ("g1","g1uri")
  , ("g1alt","g1uri")
  , ("g2","g3")
  , ("g4","g5")
  , ("g7","g8")
  , ("g7","g10")
  , ("g7","g10a")
  , ("g8","g10")
  , ("g8","g10a")
  , ("g9","g9a")
  , ("g10","g10a")
  ]

{-  
  TODO: test Foldable instance of NSGraph
  
pick one of

Methods
fold :: Monoid m => t m -> mSource

Combine the elements of a structure using a monoid.

foldMap :: Monoid m => (a -> m) -> t a -> mSource

Map each element of the structure to a monoid, and combine the results.

foldr :: (a -> b -> b) -> b -> t a -> bSource

Right-associative fold of a structure.

foldr f z = foldr f z . toList
foldl :: (a -> b -> a) -> a -> t b -> aSource

Left-associative fold of a structure.

foldl f z = foldl f z . toList
foldr1 :: (a -> a -> a) -> t a -> aSource

A variant of foldr that has no base case, and thus may only be applied to non-empty structures.

foldr1 f = foldr1 f . toList
foldl1 :: (a -> a -> a) -> t a -> aSource

A variant of foldl that has no base case, and thus may only be applied to non-empty structures.

foldl1 f = foldl1 f . toList

XXX END DIGRESSION
-}
  
testGraphEqSuite :: Test
testGraphEqSuite = TestList
  [ testGraphEq (tLab ll1 ll2) (tEq ll1 ll2) gg1 gg2
      | (ll1,gg1) <- glist , (ll2,gg2) <- glist ]
    where
    tLab ll1 ll2 = ll1 ++ "-" ++ ll2
    tEq  ll1 ll2 = (ll1 == ll2)        ||
            (ll1,ll2) `elem` grapheqlist ||
            (ll2,ll1) `elem` grapheqlist

-- Selected tests for debugging

testGraphEqSelSuite :: Test
testGraphEqSelSuite = TestList
  [ testGraphEq "g1-g2" False g1 g2
  , testGraphEq "g2-g1" False g2 g1
  , testGraphEq "g2-g2" True  g2 g2
  , testGraphEq "g2-g3" True  g2 g3
  , testGraphEq "g1-g4" False g1 g4
  , testGraphEq "g2-g4" False g2 g4
  , testGraphEq "g7-g7" True  g7 g7
  , testGraphEq "g7-g8" True  g7 g8
  , testGraphEq "g8-g7" True  g8 g7
  , testGraphEq "g9-g9a"   True g9 g9a
  , testGraphEq "g10-g10a" True g10 g10a
  ]

------------------------------------------------------------
--  Test updating formulae
------------------------------------------------------------

testFormulaLookup ::
    String -> FormulaMap RDFLabel -> RDFLabel -> Maybe RDFGraph -> Test
testFormulaLookup lab fs fl gr =
  testCompare "testFormulaLookup:" lab gr jfg
    where
      jfg = mapFindMaybe fl fs

testMaybeEq :: (Eq a, Show a) => String -> Maybe a -> Maybe a -> Test
testMaybeEq = testCompare "testMaybeEq:"

g1f1, g1f2, g1f3, g1f4, g1f5, g1f6, g1f7 :: RDFGraph
g1f1 = g1
g1f2 = setFormulae fm2 g1f1
g1f3 = setFormulae fm3 g1f1
g1f4 = setFormulae fm4 g1f1
g1f5 = setFormulae fm5 g1f1
g1f6 = setFormulae fm6 g1f1
g1f7 = setFormulae fm7 g1f1

g1f1str, g1f2str :: String

g1f1str = 
  "Graph, formulae: \n" ++
  "arcs: \n" ++
  "    (base1:s1,base1:p1,base1:o1)"

g1f2str =
  "Graph, formulae: \n    " ++
  lf22str ++ "\n" ++
  "arcs: \n" ++
  "    (base1:s1,base1:p1,base1:o1)"

lf11, lf22, lf23, lf24, lf25, lf27, lf33, lf36 :: LookupFormula RDFLabel RDFGraph
lf11 = Formula s1 g1
lf22 = newEntry (s2,g2)
lf23 = newEntry (s2,g3)
lf24 = newEntry (s2,g4)
lf25 = newEntry (s2,g5)
lf27 = newEntry (s2,g7)
lf33 = newEntry (s3,g3)
lf36 = newEntry (s3,g6)

lf22str :: String
lf22str =
  "base2:s2 :- { \n" ++ 
  "        (base1:s1,base1:p1,base1:o1)\n" ++ 
  "        (base2:s2,base1:p1,base2:o2)\n" ++ 
  "        (base3:s3,base1:p1,base3:o3)\n" ++ 
  "        (base1:s1,base1:p1,\"l1\")\n" ++ 
  "        (base2:s2,base1:p1,\"l4\"^^base1:type1)\n" ++ 
  "        (base3:s3,base1:p1,\"l10\"^^rdf:XMLLiteral) }"

fm2, fm3, fm4, fm5, fm6, fm7 :: LookupMap (LookupFormula RDFLabel RDFGraph)
fm2  = LookupMap [lf22]
fm3  = LookupMap [lf11, lf22, lf33]
fm4  = LookupMap [lf11, lf23, lf33]
fm5  = LookupMap [lf11, lf24, lf36]
fm6  = LookupMap [lf11, lf25, lf36]
fm7  = LookupMap [lf11, lf27, lf36]

f1, f2, f3, f4, f5, f6, f7 :: FormulaMap RDFLabel
f1   = getFormulae g1f1
f2   = getFormulae g1f2
f3   = getFormulae g1f3
f4   = getFormulae g1f4
f5   = getFormulae g1f5
f6   = getFormulae g1f6
f7   = getFormulae g1f7

--  Same pattern as 1-3, but using base graph with more nodes used:
--  The graph comparison results are expected to be different,
--  because of formulae associated with nodes actually used in the
--  graph

g2f1, g2f2, g2f3 :: RDFGraph
g2f1 = g2
g2f2 = setFormulae fm2 g2f1
g2f3 = setFormulae fm3 g2f1

f8, f9, f10 :: FormulaMap RDFLabel
f8   = getFormulae g2f1
f9   = getFormulae g2f2
f10  = getFormulae g2f3

--  Comparison of graphs containing formulae.
--  The intent is that graphs are matched if there is a bijection,
--  where the matched nodes are associated with matching formulae.
--  Definitions of formulae not used in the graphs don't affect the
--  match result.

--  Test methods to set/access an individual formula in a graph

g1f21, g1f22 :: RDFGraph
g1f21 = setFormula  (Formula s1 g7) g1f2
g1f22 = setFormula  (Formula s1 g1) g1f21

f21, f22 :: FormulaMap RDFLabel
f21  = getFormulae g1f21
f22  = getFormulae g1f22

f23a, f23b, f23c :: Maybe (NSGraph RDFLabel)
f23a = getFormula g1f22 s1
f23b = getFormula g1f22 s2
f23c = getFormula g1f22 s3

testGraphFormulaSuite :: Test
testGraphFormulaSuite = TestLabel "TestFormulae" $ TestList
  [ testFormulaLookup "01a" f1 s1 Nothing
  , testFormulaLookup "01b" f1 s2 Nothing
  , testFormulaLookup "01c" f1 s3 Nothing
  , testFormulaLookup "02a" f2 s1 Nothing
  , testFormulaLookup "02b" f2 s2 (Just g2)
  , testFormulaLookup "02c" f2 s3 Nothing
  , testFormulaLookup "03a" f3 s1 (Just g1)
  , testFormulaLookup "03b" f3 s2 (Just g2)
  , testFormulaLookup "03c" f3 s3 (Just g3)
  , testFormulaLookup "04a" f4 s1 (Just g1)
  , testFormulaLookup "04b" f4 s2 (Just g3)
  , testFormulaLookup "04c" f4 s3 (Just g3)
  , testFormulaLookup "05a" f5 s1 (Just g1)
  , testFormulaLookup "05b" f5 s2 (Just g4)
  , testFormulaLookup "05c" f5 s3 (Just g6)
  , testFormulaLookup "06a" f6 s1 (Just g1)
  , testFormulaLookup "06b" f6 s2 (Just g5)
  , testFormulaLookup "06c" f6 s3 (Just g6)
  , testFormulaLookup "07a" f7 s1 (Just g1)
  , testFormulaLookup "07b" f7 s2 (Just g7)
  , testFormulaLookup "07c" f7 s3 (Just g6)
  , testFormulaLookup "08a" f8 s1 Nothing
  , testFormulaLookup "08b" f8 s2 Nothing
  , testFormulaLookup "08c" f8 s3 Nothing
  , testFormulaLookup "09a" f9 s1 Nothing
  , testFormulaLookup "09b" f9 s2 (Just g2)
  , testFormulaLookup "09c" f9 s3 Nothing
  , testFormulaLookup "10a" f10 s1 (Just g1)
  , testFormulaLookup "10b" f10 s2 (Just g2)
  , testFormulaLookup "10c" f10 s3 (Just g3)
    
    -- a few tests added in to improve test coverage
  , testEq "lf11" (Formula s1 g1) lf11  
  , testEq "g2:show"    (g2str "") (show g2) 
  , testEq "g1f1:show"  g1f1str (show g1f1) 
  , testEq "g1f2:show"  g1f2str (show g1f2) 
  , testEq "lf22:show" lf22str (show lf22) 
  , testEq "[]:showList"    "[no graphs]" (show ([] :: [RDFGraph])) 
  , testEq "g2:showList1"    ("[" ++ g2str " " ++ "]") (show [g2]) 
  , testEq "g2:showList2"    ("[" ++ g2str " " ++ ",\n " ++ g2str " " ++ "]") (show [g2,g2]) 
    
    -- back to the main schedule
  , testGraphEq "g1f1-g1f1" True  g1f1 g1f1
  , testGraphEq "g1f1-g1f2" True  g1f1 g1f2
  , testGraphEq "g1f1-g1f3" False g1f1 g1f3
  , testGraphEq "g1f2-g1f1" True  g1f2 g1f1
  , testGraphEq "g1f2-g1f2" True  g1f2 g1f2
  , testGraphEq "g1f2-g1f3" False g1f2 g1f3
  , testGraphEq "g1f3-g1f1" False g1f3 g1f1
  , testGraphEq "g1f3-g1f2" False g1f3 g1f2
  , testGraphEq "g1f3-g1f3" True  g1f3 g1f3
  , testGraphEq "g1f4-g1f3" True  g1f4 g1f3
  , testGraphEq "g1f4-g1f4" True  g1f4 g1f4
  , testGraphEq "g1f4-g1f5" True  g1f4 g1f5
  , testGraphEq "g1f5-g1f5" True  g1f5 g1f5
  , testGraphEq "g1f5-g1f6" True  g1f5 g1f6
  , testGraphEq "g1f5-g1f7" True  g1f5 g1f7
  , testGraphEq "g1f6-g1f5" True  g1f6 g1f5
  , testGraphEq "g1f6-g1f6" True  g1f6 g1f6
  , testGraphEq "g1f6-g1f7" True  g1f6 g1f7
  , testGraphEq "g1f7-g1f5" True  g1f7 g1f5
  , testGraphEq "g1f7-g1f6" True  g1f7 g1f6
  , testGraphEq "g1f7-g1f7" True  g1f7 g1f7
  , testGraphEq "g2f1-g2f1" True  g2f1 g2f1
  , testGraphEq "g2f1-g2f2" False g2f1 g2f2
  , testGraphEq "g2f1-g2f3" False g2f1 g2f3
  , testGraphEq "g2f2-g2f1" False g2f2 g2f1
  , testGraphEq "g2f2-g2f2" True  g2f2 g2f2
  , testGraphEq "g2f2-g2f3" False g2f2 g2f3
  , testGraphEq "g2f3-g2f1" False g2f3 g2f1
  , testGraphEq "g2f3-g2f2" False g2f3 g2f2
  , testGraphEq "g2f3-g2f3" True  g2f3 g2f3
  , testFormulaLookup "21a" f21 s1 (Just g7)
  , testFormulaLookup "21b" f21 s2 (Just g2)
  , testFormulaLookup "21c" f21 s3 Nothing
  , testFormulaLookup "22a" f22 s1 (Just g1)
  , testFormulaLookup "22b" f22 s2 (Just g2)
  , testFormulaLookup "22c" f22 s3 Nothing
  , testMaybeEq "23a" f23a (Just g1)
  , testMaybeEq "23b" f23b (Just g2)
  , testMaybeEq "23c" f23c Nothing
  ]

------------------------------------------------------------
--  Test fmap translations of graphs, including formulae
------------------------------------------------------------

translate :: RDFLabel -> RDFLabel
translate lab
    | lab == s1 = st1
    | lab == s2 = st2
    | lab == s3 = st3
    | otherwise = lab

translateM :: RDFLabel -> Maybe RDFLabel
translateM lab
    | lab == s1   = Just st1
    | lab == s2   = Just st2
    | lab == s3   = Just st3
    | isBlank lab = Nothing
    | otherwise   = Just lab

gt1f1a, gt1f1b, gt1f2a, gt1f2b, gt1f3a, gt1f3b,
  gt2f1a, gt2f1b, gt2f2a, gt2f2b,
  gt2f3a, gt2f3b :: RDFGraph
gt1f1a = gt1
gt1f1b = fmap translate g1f1
gt1f2a = setFormulae ftm2 gt1
gt1f2b = fmap translate g1f2
gt1f3a = setFormulae ftm3 gt1
gt1f3b = fmap translate g1f3
gt2f1a = gt2
gt2f1b = fmap translate g2f1
gt2f2a = setFormulae ftm2 gt2
gt2f2b = fmap translate g2f2
gt2f3a = setFormulae ftm3 gt2
gt2f3b = fmap translate g2f3

ft1, ft2, ft3, ft4, ft5, ft6 :: FormulaMap RDFLabel
ft1 = getFormulae gt1f1b
ft2 = getFormulae gt1f2b
ft3 = getFormulae gt1f3b
ft4 = getFormulae gt2f1b
ft5 = getFormulae gt2f2b
ft6 = getFormulae gt2f3b

ftm2, ftm3 :: FormulaMap RDFLabel
ftm2   = LookupMap [Formula st2 gt2]
ftm3   = LookupMap [Formula st1 gt1,Formula st2 gt2,Formula st3 gt3]

-- Monadic translate tests, using Maybe Monad

gt1f1aM, gt1f1bM, gt1f2aM, gt1f2bM, gt1f5M :: Maybe RDFGraph
gt1f1aM = Just gt1
gt1f1bM = T.mapM translateM g1f1
gt1f2aM = Just gt1f2a
gt1f2bM = T.mapM translateM g1f2
gt1f5M = T.mapM translateM g1f5

ft1M, ft2M :: FormulaMap RDFLabel
ft1M = getFormulae $ fromJust gt1f1bM
ft2M = getFormulae $ fromJust gt1f2bM

testGraphTranslateSuite :: Test
testGraphTranslateSuite = TestLabel "TestTranslate" $ TestList
  [ testGraphEq "gt1f1a-gt1f1b" True gt1f1a gt1f1b
  , testFormulaLookup "GraphTranslate01b" ft1 st1 Nothing
  , testFormulaLookup "GraphTranslate01c" ft1 st2 Nothing
  , testFormulaLookup "GraphTranslate01d" ft1 st3 Nothing
  , testEq "gt1f1a-gt1f1b" gt1f1a gt1f1b
  , testGraphEq "gt1f2a-gt1f2b" True gt1f2a gt1f2b
  , testFormulaLookup "GraphTranslate02b" ft2 st1 Nothing
  , testFormulaLookup "GraphTranslate02c" ft2 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate02d" ft2 st3 Nothing
  , testGraphEq "gt1f3a-gt1f3b" True gt1f3a gt1f3b
  , testFormulaLookup "GraphTranslate03b" ft3 st1 (Just gt1)
  , testFormulaLookup "GraphTranslate03c" ft3 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate03d" ft3 st3 (Just gt3)
  , testGraphEq "gt2f1a-gt2f1b" True gt2f1a gt2f1b
  , testFormulaLookup "GraphTranslate04b" ft4 st1 Nothing
  , testFormulaLookup "GraphTranslate04c" ft4 st2 Nothing
  , testFormulaLookup "GraphTranslate04d" ft4 st3 Nothing
  , testGraphEq "gt2f2a-gt2f2b" True gt2f2a gt2f2b
  , testFormulaLookup "GraphTranslate05b" ft5 st1 Nothing
  , testFormulaLookup "GraphTranslate05c" ft5 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate05d" ft5 st3 Nothing
  , testGraphEq "gt2f3a-gt2f3b" True gt2f3a gt2f3b
  , testFormulaLookup "GraphTranslate06b" ft6 st1 (Just gt1)
  , testFormulaLookup "GraphTranslate06c" ft6 st2 (Just gt2)
  , testFormulaLookup "GraphTranslate06d" ft6 st3 (Just gt3)
  , testGraphEqM "gt1f1aM-gt1f1bM" True gt1f1aM gt1f1bM
  , testFormulaLookup "GraphTranslate07b" ft1M st1 Nothing
  , testFormulaLookup "GraphTranslate07c" ft1M st2 Nothing
  , testFormulaLookup "GraphTranslate07d" ft1M st3 Nothing
  , testEq "gt1f1aM-gt1f1bM" gt1f1aM gt1f1bM
  , testGraphEqM "gt1f2aM-gt1f2bM" True gt1f2aM gt1f2bM
  , testFormulaLookup "GraphTranslate08b" ft2M st1 Nothing
  , testFormulaLookup "GraphTranslate08c" ft2M st2 (Just gt2)
  , testFormulaLookup "GraphTranslate08d" ft2M st3 Nothing
  , testEq "gt1f2aM-gt1f2bM" gt1f2aM gt1f1bM
  , testEq "GraphTranslate09a" Nothing gt1f5M
  ]

------------------------------------------------------------
--  Test merge with conflicting bnodes, including formulae
------------------------------------------------------------

testMerge :: String -> RDFGraph -> RDFGraph -> RDFGraph -> Test
testMerge lab a1 a2 gr =
    TestCase ( assertGrEquiv ("testMerge:"++lab) gr (a1 `mappend` a2) )
            
assertGrEquiv :: String -> RDFGraph -> RDFGraph -> Assertion
assertGrEquiv lbl gg1 gg2 = 
  assertString $
    if (getArcs gg1) `equiv` (getArcs gg2) then ""
    else lbl++"\nExpected: "++(show gg1)++"\nObtained: "++(show gg2)

assertGrEq :: String -> RDFGraph -> RDFGraph -> Assertion
assertGrEq lbl gg1 gg2 = 
  assertString $
    if gg1 == gg2 then ""
    else lbl++"\nExpected: "++(show gg1)++"\nObtained: "++(show gg2)

testEquiv :: (Eq a) => String -> [a] -> [a] -> Test
testEquiv lab l1s l2s = TestCase $ assertBool lab (l1s `equiv` l2s)

tm01, tm02, tm03, tm04, tm05, tm06, tm07, tm08, tm09,
  tm10, tm11, tm12, tm13, tm14 :: Arc RDFLabel
tm01 = arc s1  p1 b1
tm02 = arc b1  p1 o2
tm03 = arc b1  p1 o3
tm04 = arc b2  p2 b3
tm05 = arc b3  p2 b4
tm06 = arc bb  p2 b5
tm07 = arc s2  p3 v1
tm08 = arc s3  p3 v2
tm09 = arc s4  p1 c1
tm10 = arc c2  p1 o4
tm11 = arc s4  p2 ba1
tm12 = arc ba2 p2 o4
tm13 = arc s4  p2 bn3
tm14 = arc bn4 p2 o4

tm21, tm22, tm23, tm24, tm25, tm26, tm27, tm28, tm29,
  tm30, tm31, tm32, tm33, tm34 :: Arc RDFLabel
tm21 = arc s1  p1 b6
tm22 = arc b6  p1 o2
tm23 = arc b6  p1 o3
tm24 = arc b7  p2 b8
tm25 = arc b8  p2 b9
tm26 = arc bb0 p2 b10
tm27 = arc s2  p3 v3
tm28 = arc s3  p3 v4
tm29 = arc s4  p1 c3
tm30 = arc c4  p1 o4
tm31 = arc s4  p2 ba3
tm32 = arc ba4 p2 o4
tm33 = arc s4  p2 bn5
tm34 = arc bn6 p2 o4

tm41, tm42, tm43, tm44 :: Arc RDFLabel
tm41  = arc s1  p1 b2
tm42  = arc b2  p1 o2
tm43  = arc b2  p1 o3
tm44  = arc b4  p2 b5

tm41a, tm44a :: Arc RDFLabel
tm41a = arc s1  p1 b4
tm44a = arc b5  p2 b6

tm67, tm68, tm69, tm70, tm71, tm72,
  tm73, tm74 :: Arc RDFLabel
tm67 = arc s2  p3 v3
tm68 = arc s3  p3 v4
tm69 = arc s4  p1 c3
tm70 = arc c4  p1 o4
tm71 = arc s4  p2 ba3
tm72 = arc ba4 p2 o4
tm73 = arc s4  p2 bn5
tm74 = arc bn6 p2 o4

gm0, gms, gms2, gm1, gm11, gm2, gm2f, gm22, gm3, gm3f, gm33,
  gm4, gm44 :: RDFGraph
gm0  = mempty
gms  = toGraph [arc s1 p1 o1, arc o1 p2 s3, arc s2 p3 o4]
gms2 = toGraph [arc us1 p1 o1, arc p1 p2 es1]
gm1  = toGraph [tm01,tm02,tm03,tm04,tm05,tm06,tm07,tm08
               ,tm09,tm10,tm11,tm12,tm13,tm14
               ]
gm11 = toGraph [tm01,tm02,tm03,tm04,tm05,tm06,tm07,tm08
               ,tm09,tm10,tm11,tm12,tm13,tm14
               ,tm21,tm22,tm23,tm24,tm25,tm26,tm27,tm28
               ,tm29,tm30,tm31,tm32,tm33,tm34
               ]
gm2  = toGraph [tm01]
gm2f = toGraph [tm41]
gm22 = toGraph [tm01,tm41]
gm3  = toGraph [tm04]
gm3f = toGraph [tm44]
gm33 = toGraph [tm04,tm44]
gm4  = toGraph [tm01,tm04]
gm44 = toGraph [tm01,tm04,tm41a,tm44a]

gm5, gm55 :: RDFGraph
gm5  = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm2]
        , statements = [tm01,tm02,tm03]
        }

gm55 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm2,Formula b2 gm2f]
        , statements = [tm01,tm02,tm03,tm41,tm42,tm43]
        }

gm5s :: RDFGraph
gm5s  = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm2]
        , statements = [tm01,tm02,tm03,
                        arc s1 p1 o1, arc o1 p2 s3, arc s2 p3 o4]
        }

gm6, gm66 :: RDFGraph
gm6 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula ba1 gm2,Formula bn3 gm3]
        , statements = [tm07,tm08,tm09,tm10,tm11,tm12,tm13,tm14]
        }

gm66 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap
                       [Formula ba1 gm2,Formula bn3 gm3
                       ,Formula ba3 gm2f,Formula bn5 gm3f
                       ]
        , statements = [tm07,tm08,tm09,tm10,tm11,tm12,tm13,tm14
                       ,tm67,tm68,tm69,tm70,tm71,tm72,tm73,tm74
                       ]
        }

gm456 :: RDFGraph
gm456 = NSGraph 
  { namespaces = nslist
  -- , formulae   = LookupMap [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = LookupMap []
  , statements = [tm01, tm04,
                  tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                  , arc b4 p1 o2
                  , arc b4 p1 o3
                 ]
  }

gm564 :: RDFGraph
gm564 = NSGraph 
  { namespaces = nslist
  -- , formulae   = LookupMap [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = LookupMap []
  , statements = [tm01, tm02, tm03
                  , arc b5 p2 b6
                  , tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                 ]
  }

gm645 :: RDFGraph
gm645 = NSGraph 
  { namespaces = nslist
  -- , formulae   = LookupMap [Formula b1 gm2, Formula ba1 gm2, Formula bn3 gm3]
  , formulae   = LookupMap []
  , statements = [tm07, tm08, tm09, tm10, tm11, tm12, tm13, tm14
                  , arc s1 p1 b4
                  , arc b4 p1 o2
                  , arc b4 p1 o3
                  , arc s1 p1 b5
                  , arc b6 p2 b7
                 ]
  }

tm81, tm82, tm811, tm821, tm812, tm822 :: Arc RDFLabel
tm81  = arc b1 p1 v1
tm82  = arc b2 p2 v2
tm811 = arc b1 p1 v3
tm821 = arc b2 p2 v4
tm812 = arc b1 p1 vb3
tm822 = arc b2 p2 vb4

gm82b1, gm82b2 :: [(RDFLabel,RDFLabel)]
gm82b1 = remapLabelList [v1,v2] [v1,v2,b1,b2]
gm82b2 = [(v1,v3),(v2,v4)]

gm81, gm82, gm82a, gm83, gm83a :: RDFGraph
gm81   = toGraph [tm81,tm82]
gm82   = toGraph [tm811,tm821]
gm82a  = remapLabels [v1,v2] [v1,v2,b1,b2] id gm81
gm83   = toGraph [tm811,tm821]
gm83a  = remapLabels [v1,v2] [v1,v2,b1,b2] makeBlank gm81

gm84, gm85, gm85a, gm86, gm86a :: RDFGraph
gm84  = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm81,Formula v2 gm81]
        , statements = [tm81,tm82]
        }

gm85 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm82,Formula v4 gm82]
        , statements = [tm811,tm821]
        }
gm85a = remapLabels [v1,v2] [v1,v2,b1,b2] id gm84

gm86 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 gm82,Formula vb4 gm82]
        , statements = [tm812,tm822]
        }
gm86a = remapLabels [v1,v2] [v1,v2,b1,b2] makeBlank gm84

testMergeSuite :: Test
testMergeSuite = TestList
  [ testMerge "00" gm0 gm0 gm0
  , testMerge "0s" gms gms gms
  , testMerge "0s2" gms2 gms2 gms2
  , testMerge "01" gm1 gm1 gm11
  , testMerge "02" gm2 gm2 gm22
  , testMerge "03" gm3 gm3 gm33
  , testMerge "04" gm4 gm4 gm44
  , testMerge "05" gm5 gm5 gm55
  , testMerge "06" gm6 gm6 gm66
  , testMerge "0+s" gm0 gms gms
  , testMerge "s+0" gms gm0 gms
  , testMerge "0+5" gm0 gm5 gm5
  , testMerge "5+0" gm5 gm0 gm5
  , testMerge "5+s" gm5 gms gm5s
  , testMerge "s+5" gms gm5 gm5s
  , TestCase (assertGrEquiv "mconcat:456" gm456 (mconcat [gm4,gm5,gm6]))
  , TestCase (assertGrEquiv "mconcat:564" gm564 (mconcat [gm5,gm6,gm4]))
  , TestCase (assertGrEquiv "mconcat:645" gm645 (mconcat [gm6,gm4,gm5]))
  , TestCase (assertGrEq "mappend"  
              (mappend gm4 (mappend gm5 gm6))
              (mappend (mappend gm4 gm5) gm6))
  , testGraphEq "Remap07" True gm82 gm82a
  , testEquiv "testRemapList07" gm82b2 gm82b1
  , testGraphEq "Remap08" True gm83 gm83a
  , testGraphEq "Remap09" True gm85 gm85a
  , testGraphEq "Remap10" True gm86 gm86a
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ testLangEqSuite
  , testConversionSuite
  , testNodeEqSuite
  , testNodeClassSuite
  , testNodeLocalSuite
  , testNewNodeSuite
  , testNodeOrdSuite
  , testLabelOtherSuite
  , testStmtEqSuite
  , testGraphEqSuite
  , testGraphEqSelSuite
  , testGraphFormulaSuite
  , testGraphTranslateSuite
  , testMergeSuite
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

geq  = testGraphEqSuite
nord = testNodeOrdSuite
gtr  = testGraphTranslateSuite

gmm g1 g2 = grMatchMap g1 g2
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
