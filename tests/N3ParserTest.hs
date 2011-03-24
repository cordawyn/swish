--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3ParserTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module contains test cases for module "N3Parser".
--
--------------------------------------------------------------------------------

module Main where

import Swish.HaskellRDF.N3Parser
    ( parseN3fromString
    , parseTextFromString, parseAltFromString
    , parseNameFromString, parsePrefixFromString
    , parseAbsURIrefFromString, parseLexURIrefFromString
    , parseURIref2FromString
    )

import Swish.HaskellRDF.RDFGraph
    ( RDFGraph, RDFLabel(..), NSGraph(..)
    -- LookupNamespace(..), Namespace
    , emptyNamespaceMap
    , LookupFormula(..), emptyFormulaMap
    , emptyRDFGraph
      -- Export selected RDFLabel values
    , res_rdf_type, res_rdf_first, res_rdf_rest, res_rdf_nil
    , res_owl_sameAs, res_log_implies
    )

import Swish.HaskellUtils.Namespace
    ( Namespace(..)
    , nullNamespace
    , ScopedName(..)
    , makeScopedName
    , nullScopedName
    )

import Swish.HaskellRDF.Vocabulary
    ( namespaceRDF
    , langName
    , rdf_XMLLiteral
    )

import Swish.HaskellUtils.LookupMap (LookupMap(..))

import Swish.HaskellRDF.GraphClass (Arc, arc) 

import Swish.HaskellUtils.ErrorM (ErrorM(..))

import Test.HUnit (Test(TestCase,TestList), assertEqual, runTestTT)

import Data.List (intercalate)

------------------------------------------------------------
--  Generic item parsing test wrapper
------------------------------------------------------------

type ParseFromString a = String -> (Either String a)

parseItemTest :: (Eq a, Show a) => ParseFromString a -> a
                 -> String -> String -> a -> String -> Test
parseItemTest ifroms def lab inp val err =
    TestList
      [ TestCase ( assertEqual ("parseItemError:"++lab) fixerr pe )
      , TestCase ( assertEqual ("parseItemValue:"++lab) val pv )
      ]
    where
        (pe,pv) = case ifroms inp of
            Left  e -> (e,def)
            Right v -> (noError,v)
        fixerr = if err /= noError then pe else noError

noError, errorText :: String
noError   = ""
errorText = "*"

------------------------------------------------------------
--  Common test wrappers
------------------------------------------------------------

testLabelEq :: String -> Bool -> RDFLabel -> RDFLabel -> Test
testLabelEq lab eq n1 n2 =
    TestCase ( assertEqual ("testLabelEq:"++lab) eq (n1==n2) )

testGraphEq :: String -> Bool -> RDFGraph -> RDFGraph -> Test
testGraphEq lab eq gg1 gg2 =
    TestCase ( assertEqual ("testGraphEq:"++lab) eq (gg1==gg2) )

parseTest :: String -> String -> RDFGraph -> String -> Test
parseTest lab inp gr er =
    TestList
      [ TestCase ( assertEqual ("parseTestError:"++lab) er pe )
      , TestCase ( assertEqual ("parseTestGraph:"++lab) gr pg )
      ]
    where
        (pe,pg) = case parseN3fromString inp of
            Result g -> ("",g)
            Error  s -> (s,emptyRDFGraph)

------------------------------------------------------------
--  Test simple character parsing
------------------------------------------------------------

parseCharTest :: String -> String
                 -> String -> String -> String -> Test
parseCharTest c = parseItemTest (parseTextFromString c) ""

parseAltTest :: String -> String
                -> String -> String -> String -> String -> Test
parseAltTest cc1 cc2 = parseItemTest (parseAltFromString cc1 cc2) ""

charInp01, char01 :: String
charInp01 = ":"
char01    = ":"

charInp02, char02 :: String
charInp02 = "<>"
char02    = "<>"

charInp03 :: String
charInp03 = "<="

charTestSuite :: Test
charTestSuite = TestList
  [ parseCharTest char01 "parseCharTest01" charInp01 char01 noError
  , parseCharTest char02 "parseCharTest02" charInp02 char02 noError
  , parseAltTest char01 char02 "parseCharTest03" charInp01 char01 noError
  , parseAltTest char01 char02 "parseCharTest04" charInp02 char02 noError
  , parseAltTest char01 char02 "parseCharTest04" charInp03 "" errorText
  ]

------------------------------------------------------------
--  Test simple name parsing
------------------------------------------------------------

parseNameTest :: String -> String -> String -> String -> Test
parseNameTest = parseItemTest parseNameFromString ""

nameTestSuite :: Test
nameTestSuite = TestList
  [ parseNameTest "parseNameTest01" "name" "name" ""
  , parseNameTest "parseNameTest02" "rdf" "rdf" ""
  ]

------------------------------------------------------------
--  Test simple prefix parsing
------------------------------------------------------------

parsePrefixTest :: String -> String -> Namespace -> String -> Test
parsePrefixTest = parseItemTest parsePrefixFromString nullNamespace

prefix01, prefix02 :: Namespace
prefix01 = Namespace "pref" "pref:"
prefix02 = Namespace "rdf" $ nsURI namespaceRDF

prefixTestSuite :: Test
prefixTestSuite = TestList
  [ parsePrefixTest "parsePrefixTest01" "pref" prefix01 ""
  , parsePrefixTest "parsePrefixTest02" "rdf" prefix02 ""
  ]

------------------------------------------------------------
--  Test absolute URIref parsing
------------------------------------------------------------

parseAbsUriRefTest :: String -> String -> String -> String -> Test
parseAbsUriRefTest = parseItemTest parseAbsURIrefFromString ""

parseLexUriRefTest :: String -> String -> String -> String -> Test
parseLexUriRefTest = parseItemTest parseLexURIrefFromString ""

absUriRefInp01, absUriRefInp01s, absUriRef01 :: String
absUriRefInp01  = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
absUriRefInp01s = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "
absUriRef01     = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

absUriRefInp02, absUriRefInp02s, absUriRef02 :: String
absUriRefInp02  = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1>"
absUriRefInp02s = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1> "
absUriRef02     = "http://id.ninebynine.org/wip/2003/test/graph1/node#s1"

absUriRefTestSuite :: Test
absUriRefTestSuite = TestList
  [ parseAbsUriRefTest "parseAbsUriRefTest01" absUriRefInp01 absUriRef01 ""
  , parseAbsUriRefTest "parseAbsUriRefTest02" absUriRefInp02 absUriRef02 ""
  , parseLexUriRefTest "parseAbsUriRefTest03" absUriRefInp01s absUriRef01 ""
  , parseLexUriRefTest "parseAbsUriRefTest04" absUriRefInp02s absUriRef02 ""
  ]

------------------------------------------------------------
--  Test simple URIref parsing
------------------------------------------------------------

parseUriRef2Test :: String -> String -> ScopedName -> String -> Test
parseUriRef2Test = parseItemTest parseURIref2FromString nullScopedName

uriRef01 :: String
uriRef01 = "rdf:type "

sname01 :: ScopedName
sname01  = ScopedName namespaceRDF "type"

uriRef02 :: String
uriRef02 = "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1> "

sname02 :: ScopedName
sname02  =
    makeScopedName "" "http://id.ninebynine.org/wip/2003/test/graph1/node#" "s1"

uriRef2TestSuite :: Test
uriRef2TestSuite = TestList
  [ parseUriRef2Test "parseUriRef2Test01" uriRef01 sname01 ""
  , parseUriRef2Test "parseUriRef2Test02" uriRef02 sname02 ""
  ]

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

base1, base2, base3, base4, basea :: Namespace
base1 = Namespace "base1" "http://id.ninebynine.org/wip/2003/test/graph1/node/"
base2 = Namespace "base2" "http://id.ninebynine.org/wip/2003/test/graph2/node#"
base3 = Namespace "base3" "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = Namespace "base4" "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"
basea = Namespace "a" "http://example.org/basea#"

s1, s2, s3, sa :: RDFLabel
s1 = Res $ ScopedName base1 "s1"
s2 = Res $ ScopedName base2 "s2"
s3 = Res $ ScopedName base3 "s3"
sa = Res $ ScopedName basea "a"

b1, b2, b3, b4, b5, b6, b7, b8 :: RDFLabel
b1 = Blank "b1"
b2 = Blank "b2"
b3 = Blank "b3"
b4 = Blank "b4"
b5 = Blank "b5"
b6 = Blank "b6"
b7 = Blank "b7"
b8 = Blank "b8"

c1, c2, c3, c4, c5, c6 :: RDFLabel
c1 = Blank "c1"
c2 = Blank "c2"
c3 = Blank "c3"
c4 = Blank "c4"
c5 = Blank "c5"
c6 = Blank "c6"

p1, p2, p3, pa :: RDFLabel
p1 = Res $ ScopedName base1 "p1" 
p2 = Res $ ScopedName base2 "p2" 
p3 = Res $ ScopedName base3 "p3" 
pa = Res $ ScopedName basea "b" 

o1, o2, o3, oa :: RDFLabel
o1 = Res $ ScopedName base1 "o1"
o2 = Res $ ScopedName base2 "o2"
o3 = Res $ ScopedName base3 "o3"
oa = Res $ ScopedName basea "c"

l1, l2, l3 :: RDFLabel
l1 = Lit "l1"  Nothing
l2 = Lit "l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'" Nothing
l3 = Lit "l3--\r\"'\\--\x0020\&--\x00A0\&--" Nothing

lfr, lxml, lfrxml :: RDFLabel
lfr    = Lit "chat"          (Just $ langName "fr")
lxml   = Lit "<br/>"         (Just rdf_XMLLiteral )
lfrxml = Lit "<em>chat</em>" (Just rdf_XMLLiteral )

f1, f2 :: RDFLabel
f1 = Res $ ScopedName base1 "f1"
f2 = Res $ ScopedName base2 "f2" 

v1, v2, v3, v4 :: RDFLabel
v1 = Var "var1"
v2 = Var "var2"
v3 = Var "var3"
v4 = Var "var4"

------------------------------------------------------------
--  Construct graphs for testing
------------------------------------------------------------

t01 , t01b, t02, t03, t04, t05, t06, t07 :: Arc RDFLabel
t01  = arc s1 p1 o1
t01b = arc b1 b2 b3
t02  = arc s2 p1 o2
t03  = arc s3 p1 o3
t04  = arc s1 p1 l1
t05  = arc s2 p1 b1
t06  = arc s3 p1 l2
t07  = arc s3 p2 l3

makeNewPrefixNamespace :: (String,Namespace) -> Namespace
makeNewPrefixNamespace (pre,ns) = Namespace pre (nsURI ns)

nslist :: LookupMap Namespace
nslist = LookupMap $ map makeNewPrefixNamespace
    [ ("base1",base1)
    , ("base2",base2)
    , ("base3",base3)
    , ("base4",base4)
    ]

g1 :: RDFGraph
g1 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01]
        }

g1a :: RDFGraph
g1a = g1 { statements = [arc sa pa oa] }

g1b :: RDFGraph
g1b = g1 { statements = [t01b] }

g2 :: RDFGraph
g2 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t02,t03]
        }

g3 :: RDFGraph
g3 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t04]
        }

g4 :: RDFGraph
g4 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t05]
        }

g5 :: RDFGraph
g5 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t02,t03,t04,t05]
        }

g6 :: RDFGraph
g6 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t06]
        }

g7 :: RDFGraph
g7 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t01,t07]
        }

t801, t802, t807, t808, t811, t812 :: Arc RDFLabel
t801 = arc s1 res_rdf_type       o1
t802 = arc s2 res_owl_sameAs     o2
-- t803 = arc s3 res_operator_plus  o3
-- t804 = arc s3 res_operator_minus o3
-- t805 = arc s3 res_operator_star  o3
-- t806 = arc s3 res_operator_slash o3
t807 = arc o1 p1 s1
t808 = arc s2 p1 o2
-- t809 = arc s1 p2 o1
-- t810 = arc o2 p2 s2
t811 = arc s1 res_log_implies o1
t812 = arc o2 res_log_implies s2

g8 :: RDFGraph
g8 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        -- , statements = [t801,t802,t803,t804,t805,t806,t807,t808,t809,t810]
        , statements = [t801,t802,t807,t808,t811,t812]
        }

g81 :: RDFGraph
g81 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t801,t802]
        }

{-
g82 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t803,t804,t805,t806]
        }
-}

g83 :: RDFGraph
g83 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        -- , statements = [t807,t808,t809,t810]
        , statements = [t807,t808,t811,t812]
        }

t911, t912, t913, t914, t921, t922, t923, t924,
  t925, t926, t927, t928 :: Arc RDFLabel
t911 = arc s1 p1 o1
t912 = arc s1 p1 o2
t913 = arc s1 p2 o2
t914 = arc s1 p2 o3
t921 = arc s2 p1 o1
t922 = arc s2 p1 o2
t923 = arc s2 p1 o3
t924 = arc s2 p1 l1
t925 = arc s2 p2 o1
t926 = arc s2 p2 o2
t927 = arc s2 p2 o3
t928 = arc s2 p2 l1

g9 :: RDFGraph
g9 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t911,t912,t913,t914,
                        t921,t922,t923,t924,
                        t925,t926,t927,t928]
        }

t1011, t1012, t1013, t1014, t1021, t1022, t1023, t1024,
  t1025, t1026, t1027, t1028 :: Arc RDFLabel
t1011 = arc s1 p1 o1
t1012 = arc o2 p1 s1
t1013 = arc s1 p2 o2
t1014 = arc o3 p2 s1
t1021 = arc s2 p1 o1
t1022 = arc s2 p1 o2
t1023 = arc s2 p1 o3
t1024 = arc s2 p1 l1
t1025 = arc o1 p2 s2
t1026 = arc o2 p2 s2
t1027 = arc o3 p2 s2
t1028 = arc l1 p2 s2

g10 :: RDFGraph
g10 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t1011,t1012,t1013,t1014,
                        t1021,t1022,t1023,t1024,
                        t1025,t1026,t1027,t1028]
        }

t1111, t1112, t1113 :: Arc RDFLabel
t1111 = arc s1 p1 v1
t1112 = arc v2 p1 o1
t1113 = arc v3 p1 v4

g11 :: RDFGraph
g11 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t1111,t1112,t1113]
        }

t1211, t1221, t1222, t1223, t1224 :: Arc RDFLabel
t1211 = arc b1 p1 o1
t1221 = arc b2 res_rdf_first v1
t1222 = arc b2 res_rdf_rest  b3
t1223 = arc b3 res_rdf_first v2
t1224 = arc b3 res_rdf_rest  res_rdf_nil

g12 :: RDFGraph
g12 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t1211,t1221,t1222,t1223,t1224]
        }

t1711, t1722, t1733 :: Arc RDFLabel
t1711 = arc s1 p1 lfr
t1722 = arc s2 p2 lxml
t1733 = arc s3 p3 lfrxml

g17 :: RDFGraph
g17 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [t1711,t1722,t1733]
        }

tx101, tx102, tx111, tx112, tx113, tx114, tx121, tx122, tx123,
  tx124, tx125, tx126, tx127, tx128 :: Arc RDFLabel
tx101 = arc b1 res_owl_sameAs s1
tx102 = arc s2 res_owl_sameAs b2
tx111 = arc b1 p1 o1
tx112 = arc b1 p1 o2
tx113 = arc b1 p2 o2
tx114 = arc b1 p2 o3
tx121 = arc b2 p1 o1
tx122 = arc b2 p1 o2
tx123 = arc b2 p1 o3
tx124 = arc b2 p1 l1
tx125 = arc b2 p2 o1
tx126 = arc b2 p2 o2
tx127 = arc b2 p2 o3
tx128 = arc b2 p2 l1

x1 :: RDFGraph
x1 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx101,tx102,
                        tx111,tx112,tx113,tx114,
                        tx121,tx122,tx123,tx124,
                        tx125,tx126,tx127,tx128]
        }

tx201, tx202, tx211, tx212, tx213, tx214, tx221, tx222, tx223,
  tx224, tx225, tx226, tx227, tx228 :: Arc RDFLabel
tx201 = arc b1 res_owl_sameAs s1
tx202 = arc s2 res_owl_sameAs b2
tx211 = arc b1 p1 o1
tx212 = arc o2 p1 b1
tx213 = arc b1 p2 o2
tx214 = arc o3 p2 b1
tx221 = arc b2 p1 o1
tx222 = arc b2 p1 o2
tx223 = arc b2 p1 o3
tx224 = arc b2 p1 l1
tx225 = arc o1 p2 b2
tx226 = arc o2 p2 b2
tx227 = arc o3 p2 b2
tx228 = arc l1 p2 b2

x2 :: RDFGraph
x2 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx201,tx202,
                        tx211,tx212,tx213,tx214,
                        tx221,tx222,tx223,tx224,
                        tx225,tx226,tx227,tx228]
        }

tx311, tx312, tx313, tx314, tx321, tx322, tx323,
  tx324, tx325, tx326, tx327, tx328 :: Arc RDFLabel
tx311 = arc s1 p1 o1
tx312 = arc o2 p1 s1
tx313 = arc s1 p2 o2
tx314 = arc o3 p2 s1
tx321 = arc s2 p1 o1
tx322 = arc s2 p1 o2
tx323 = arc s2 p1 o3
tx324 = arc s2 p1 l1
tx325 = arc o1 p2 s2
tx326 = arc o2 p2 s2
tx327 = arc o3 p2 s2
tx328 = arc l1 p2 s2

x3 :: RDFGraph
x3 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx311,tx312,tx313,tx314,
                        tx321,tx322,tx323,tx324,
                        tx325,tx326,tx327,tx328]
        }

tx401, tx402, tx403, tx404, tx405, tx406, tx407,
  tx408, tx409 :: Arc RDFLabel
tx401 = arc s1 res_owl_sameAs b1
tx402 = arc b1 res_rdf_first  o1
tx403 = arc b1 res_rdf_rest   b2
tx404 = arc b2 res_rdf_first  o2
tx405 = arc b2 res_rdf_rest   b3
tx406 = arc b3 res_rdf_first  o3
tx407 = arc b3 res_rdf_rest   b4
tx408 = arc b4 res_rdf_first  l1
tx409 = arc b4 res_rdf_rest   res_rdf_nil

x4 :: RDFGraph
x4 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx401,tx402,tx403,tx404,
                        tx405,tx406,tx407,tx408,
                        tx409]
        }

tx501, tx502, tx503, tx504, tx505, tx506, tx507,
  tx508, tx509 :: Arc RDFLabel
tx501 = arc b1 res_owl_sameAs s1
tx502 = arc b1 res_rdf_first  o1
tx503 = arc b1 res_rdf_rest   b2
tx504 = arc b2 res_rdf_first  o2
tx505 = arc b2 res_rdf_rest   b3
tx506 = arc b3 res_rdf_first  o3
tx507 = arc b3 res_rdf_rest   b4
tx508 = arc b4 res_rdf_first  l1
tx509 = arc b4 res_rdf_rest   res_rdf_nil

x5 :: RDFGraph
x5 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx501,tx502,tx503,tx504,
                        tx505,tx506,tx507,tx508,
                        tx509]
        }

tx601, tx602, tx603, tx604, tx605, tx606, tx607,
  tx608 :: Arc RDFLabel
tx601 = arc s1 res_rdf_first o1
tx602 = arc s1 res_rdf_rest  b2
tx603 = arc b2 res_rdf_first o2
tx604 = arc b2 res_rdf_rest  b3
tx605 = arc b3 res_rdf_first o3
tx606 = arc b3 res_rdf_rest  b4
tx607 = arc b4 res_rdf_first l1
tx608 = arc b4 res_rdf_rest  res_rdf_nil

x6 :: RDFGraph
x6 = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx601,tx602,tx603,tx604,
                        tx605,tx606,tx607,tx608]
        }

tx701 :: Arc RDFLabel
tx701 = arc b1 p2 f2

x7 :: RDFGraph
x7    = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 g2]
        , statements = [tx701]
        }

tx801 :: Arc RDFLabel
tx801 = arc f1 p2 f2

x8 :: RDFGraph
x8    = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula f1 g2]
        , statements = [tx801]
        }

x9 :: RDFGraph
x9    = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula f1 g1]
        , statements = [tx801]
        }

--  Test allocation of bnodes carries over a nested formula

tx1201, tx1202, tx1203, tx1204, tx1205, 
  tx1211, tx1212 :: Arc RDFLabel
tx1201 = arc s1 p1 b1
tx1202 = arc b1 p1 o1
tx1203 = arc b2 p2 f2
tx1204 = arc s3 p3 b3
tx1205 = arc b3 p3 o3
tx1211 = arc s2 p2 b4
tx1212 = arc b4 p2 o2

x12fg :: RDFGraph
x12fg  = NSGraph
        { namespaces = emptyNamespaceMap
        , formulae   = emptyFormulaMap
        , statements = [tx1211,tx1212]
        }
        
x12 :: RDFGraph
x12    = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b2 x12fg]
        , statements = [tx1201,tx1202,tx1203,tx1204,tx1205]
        }

--  List of simple anon nodes

tx1301, tx1302, tx1303, tx1304, tx1305, tx1306, tx1307,
  tx1308, tx1309 :: Arc RDFLabel
tx1301 = arc s1 res_rdf_first b1
tx1302 = arc s1 res_rdf_rest  c1
tx1303 = arc c1 res_rdf_first b2
tx1304 = arc c1 res_rdf_rest  c2
tx1305 = arc c2 res_rdf_first b3
tx1306 = arc c2 res_rdf_rest  res_rdf_nil
tx1307 = arc b1 p1 o1
tx1308 = arc b2 p1 o2
tx1309 = arc b3 p1 o3

x13 :: RDFGraph
x13    = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx1301,tx1302,tx1303,tx1304,tx1305,tx1306,
                        tx1307,tx1308,tx1309]
        }

--  List of more complex anon nodes

tx1401, tx1402, tx1403, tx1404, tx1405, tx1406, tx1407,
  tx1408, tx1409, tx1410, tx1411, tx1412 :: Arc RDFLabel
tx1401 = arc s1 res_rdf_first b1
tx1402 = arc s1 res_rdf_rest  c1
tx1403 = arc c1 res_rdf_first b2
tx1404 = arc c1 res_rdf_rest  c2
tx1405 = arc c2 res_rdf_first b3
tx1406 = arc c2 res_rdf_rest  res_rdf_nil
tx1407 = arc b1 p1 o1
tx1408 = arc b1 p2 o1
tx1409 = arc b2 p1 o2
tx1410 = arc b2 p2 o2
tx1411 = arc b3 p1 o3
tx1412 = arc b3 p2 o3

x14 :: RDFGraph
x14    = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx1401,tx1402,tx1403,tx1404,tx1405,tx1406,
                        tx1407,tx1408,tx1409,tx1410,tx1411,tx1412]
        }

--  List with nested list

tx1501, tx1502, tx1503, tx1504, tx1505, tx1506, tx1507,
  tx1508, tx1509 :: Arc RDFLabel
tx1501 = arc s1 res_rdf_first b1
tx1502 = arc s1 res_rdf_rest  c1
tx1503 = arc c1 res_rdf_first b2
tx1504 = arc c1 res_rdf_rest  c2
tx1505 = arc c2 res_rdf_first b3
tx1506 = arc c2 res_rdf_rest  res_rdf_nil
tx1507 = arc b1 p1 o1
tx1508 = arc b2 p2 c3
tx1509 = arc b3 p1 o3

tx1521, tx1522, tx1523, tx1524, tx1525, tx1526, tx1527,
  tx1528, tx1529 :: Arc RDFLabel
tx1521 = arc c3 res_rdf_first b4
tx1522 = arc c3 res_rdf_rest  c4
tx1523 = arc c4 res_rdf_first b5
tx1524 = arc c4 res_rdf_rest  c5
tx1525 = arc c5 res_rdf_first b6
tx1526 = arc c5 res_rdf_rest  res_rdf_nil
tx1527 = arc b4 p1 o1
tx1528 = arc b5 p1 o2
tx1529 = arc b6 p1 o3

x15 :: RDFGraph
x15    = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx1501,tx1502,tx1503,tx1504,tx1505,tx1506,
                        tx1507,tx1508,tx1509,
                        tx1521,tx1522,tx1523,tx1524,tx1525,tx1526,
                        tx1527,tx1528,tx1529]
        }

--  More complex list with nested list

tx1601, tx1602, tx1603, tx1604, tx1605, tx1606, tx1607,
  tx1608, tx1609, tx1610, tx1611 :: Arc RDFLabel
tx1601 = arc s1 res_rdf_first b1
tx1602 = arc s1 res_rdf_rest  c1
tx1603 = arc c1 res_rdf_first b2
tx1604 = arc c1 res_rdf_rest  c2
tx1605 = arc c2 res_rdf_first b3
tx1606 = arc c2 res_rdf_rest  res_rdf_nil
tx1607 = arc b1 p1 o1
tx1608 = arc b1 p2 o1
tx1609 = arc b2 p2 c3
tx1610 = arc b3 p1 o3
tx1611 = arc b3 p2 o3

tx1621, tx1622, tx1623, tx1624, tx1625, tx1626, tx1627,
  tx1628, tx1629, tx1630, tx1631, tx1632 :: Arc RDFLabel
tx1621 = arc c3 res_rdf_first b4
tx1622 = arc c3 res_rdf_rest  c4
tx1623 = arc c4 res_rdf_first b5
tx1624 = arc c4 res_rdf_rest  c5
tx1625 = arc c5 res_rdf_first b6
tx1626 = arc c5 res_rdf_rest  res_rdf_nil
tx1627 = arc b4 p1 o1
tx1628 = arc b4 p2 o1
tx1629 = arc b5 p1 o2
tx1630 = arc b5 p2 o2
tx1631 = arc b6 p1 o3
tx1632 = arc b6 p2 o3

x16 :: RDFGraph
x16    = NSGraph
        { namespaces = nslist
        , formulae   = emptyFormulaMap
        , statements = [tx1601,tx1602,tx1603,tx1604,tx1605,tx1606,
                        tx1607,tx1608,tx1609,tx1610,tx1611,
                        tx1621,tx1622,tx1623,tx1624,tx1625,tx1626,
                        tx1627,tx1628,tx1629,tx1630,tx1631,tx1632]
        }

------------------------------------------------------------
--  Simple parser tests
------------------------------------------------------------

commonPrefixes :: String
commonPrefixes =
    "@prefix base1 : <" ++ nsURI base1 ++ "> . \n" ++
    "@prefix base2 : <" ++ nsURI base2 ++ "> . \n" ++
    "@prefix base3 : <" ++ nsURI base3 ++ "> . \n"

--  Single statement using <uri> form
simpleN3Graph_g1_01 :: String
simpleN3Graph_g1_01 =
    " <http://id.ninebynine.org/wip/2003/test/graph1/node/s1> " ++
    " <http://id.ninebynine.org/wip/2003/test/graph1/node/p1> " ++
    " <http://id.ninebynine.org/wip/2003/test/graph1/node/o1> . "

--  Single statement using prefix:name form
simpleN3Graph_g1_02 :: String
simpleN3Graph_g1_02 =
    "@prefix base1 : <" ++ nsURI base1 ++ "> ." ++
    " base1:s1 base1:p1 base1:o1 . "

--  Single statement using prefix:name form
--  (this was added to check that the parser did not
--   think we meant 'a:a a :b .' here)
--
simpleN3Graph_g1_02a :: String
simpleN3Graph_g1_02a =
    "@prefix a: <" ++ nsURI basea ++ "> ." ++
    "a:a a:b a:c ."

--  Single statement using :name form
simpleN3Graph_g1_03 :: String
simpleN3Graph_g1_03 =
    "@prefix : <" ++ nsURI base1 ++ "> .\n" ++
    " :s1 :p1 :o1 . "

--  Single statement using relative URI form
simpleN3Graph_g1_04 :: String
simpleN3Graph_g1_04 =
    "@base <" ++ nsURI base1 ++ "> .\n" ++
    " <s1> <p1> <o1> . "

--  Single statement using blank nodes
simpleN3Graph_g1_05 :: String
simpleN3Graph_g1_05 =
    "@base <" ++ nsURI base1 ++ "> .\n" ++
    " _:b1 _:b2 _:b3 . "

--  Single statement with junk following
simpleN3Graph_g1_06 :: String
simpleN3Graph_g1_06 =
    "@prefix base1 : <" ++ nsURI base1 ++ "> ." ++
    " base1:s1 base1:p1 base1:o1 . " ++
    " **** "

--  Multiple statements
simpleN3Graph_g2 :: String
simpleN3Graph_g2 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base2:s2 base1:p1 base2:o2 . \n" ++
    " base3:s3 base1:p1 base3:o3 . \n"

--  Graph with literal
simpleN3Graph_g3 :: String
simpleN3Graph_g3 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base1:s1 base1:p1 \"l1\" . \n"

--  Graph with nodeid
simpleN3Graph_g4 :: String
simpleN3Graph_g4 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base2:s2 base1:p1 _:b1 . \n"

--  Graph with literal and nodeid
simpleN3Graph_g5 :: String
simpleN3Graph_g5 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base2:s2 base1:p1 base2:o2 . \n" ++
    " base3:s3 base1:p1 base3:o3 . \n" ++
    " base1:s1 base1:p1 \"l1\" . \n"   ++
    " base2:s2 base1:p1 _:b1 . \n"

--  Triple-quoted literal
simpleN3Graph_g6 :: String
simpleN3Graph_g6 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base3:s3 base1:p1 \"\"\"l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'\"\"\" . \n"

--  String escapes
simpleN3Graph_g7 :: String
simpleN3Graph_g7 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 . \n" ++
    " base3:s3 base2:p2 " ++
    " \"l3--\\r\\\"\\'\\\\--\\u0020--\\U000000A0--\" " ++
    " . \n"

--  Different verb forms
simpleN3Graph_g8 :: String
simpleN3Graph_g8 =
    commonPrefixes ++
    " base1:s1 a base1:o1 . \n" ++
    " base2:s2 = base2:o2 . \n" ++
    -- " base3:s3 + base3:o3 . \n" ++
    -- " base3:s3 - base3:o3 . \n" ++
    -- " base3:s3 * base3:o3 . \n" ++
    -- " base3:s3 / base3:o3 . \n" ++
    " base1:s1 @is  base1:p1 @of base1:o1 . \n" ++
    " base2:s2 @has base1:p1 base2:o2 . \n" ++
    -- " base1:s1 >-  base2:p2 -> base1:o1 . \n" ++
    -- " base2:s2 <-  base2:p2 <- base2:o2 . \n"
    " base1:s1 => base1:o1 . \n" ++
    " base2:s2 <= base2:o2 . \n"

simpleN3Graph_g81 :: String
simpleN3Graph_g81 =
    commonPrefixes ++
    " base1:s1 a base1:o1 . \n" ++
    " base2:s2 = base2:o2 . \n"

{-
simpleN3Graph_g82 =
    commonPrefixes ++
    " base3:s3 + base3:o3 . \n" ++
    " base3:s3 - base3:o3 . \n" ++
    " base3:s3 * base3:o3 . \n" ++
    " base3:s3 / base3:o3 . \n"
-}

simpleN3Graph_g83 :: String
simpleN3Graph_g83 =
    commonPrefixes ++
    " base1:s1 @is  base1:p1 @of base1:o1 . \n" ++
    " base2:s2 @has base1:p1 base2:o2 . \n" ++
    -- " base1:s1 >-  base2:p2 -> base1:o1 . \n" ++
    -- " base2:s2 <-  base2:p2 <- base2:o2 . \n"
    " base1:s1 => base1:o1 . \n" ++
    " base2:s2 <= base2:o2 . \n"

--  Semicolons and commas
simpleN3Graph_g9 :: String
simpleN3Graph_g9 =
    commonPrefixes ++
    " base1:s1 base1:p1 base1:o1 ; \n" ++
    "          base1:p1 base2:o2 ; \n" ++
    "          base2:p2 base2:o2 ; \n" ++
    "          base2:p2 base3:o3 . \n" ++
    " base2:s2 base1:p1 base1:o1 , \n" ++
    "                   base2:o2 , \n" ++
    "                   base3:o3 , \n" ++
    "                   \"l1\"   ; \n" ++
    "          base2:p2 base1:o1 , \n" ++
    "                   base2:o2 , \n" ++
    "                   base3:o3 , \n" ++
    "                   \"l1\"   . \n"

--  'is ... of' and semicolons and commas
simpleN3Graph_g10 :: String
simpleN3Graph_g10 =
    commonPrefixes ++
    " base1:s1 @has base1:p1 base1:o1 ; \n" ++
    "          @is  base1:p1 @of base2:o2 ; \n" ++
    "          @has base2:p2 base2:o2 ; \n" ++
    "          @is  base2:p2 @of base3:o3 . \n" ++
    " base2:s2 @has base1:p1 base1:o1 , \n" ++
    "                        base2:o2 , \n" ++
    "                        base3:o3 , \n" ++
    "                        \"l1\"   ; \n" ++
    "          @is  base2:p2 @of base1:o1 , \n" ++
    "                          base2:o2 , \n" ++
    "                          base3:o3 , \n" ++
    "                          \"l1\"   . \n"

--  Simple statements using ?var form
simpleN3Graph_g11 :: String
simpleN3Graph_g11 =
    "@prefix base1 : <" ++ nsURI base1 ++ "> . \n" ++
    " base1:s1 base1:p1 ?var1 . \n"          ++
    " ?var2 base1:p1 base1:o1 . \n"          ++
    " ?var3 base1:p1 ?var4 .    \n"

--  Bare anonymous nodes
simpleN3Graph_g12 :: String
simpleN3Graph_g12 =
    "@prefix base1 : <" ++ nsURI base1 ++ "> . \n" ++
    " [ base1:p1 base1:o1 ] .  \n"          ++
    " ( ?var1 ?var2 ) .    \n"

--  Literals with dataype and language
simpleN3Graph_g17 :: String
simpleN3Graph_g17 =
    commonPrefixes ++
    " base1:s1 base1:p1 \"chat\"@fr . \n "                          ++
    " base2:s2 base2:p2 \"<br/>\"^^rdf:XMLLiteral . \n "            ++
    " base3:s3 base3:p3 \"<em>chat</em>\"^^rdf:XMLLiteral . \n "

emsg16 :: String
emsg16 = intercalate "\n" [
  "",
  "@prefix base1 : <http://id.ninebynine.org/wip/2003/test/graph1/node/> . base1:s1 base1:p1 base1:o1 .  **** ",
  "                                                                                                      ^",
  "(line 1, column 103 indicated by the '^' sign above):",
  "",
  "unexpected \"*\"",
  "expecting declaration, pathitem or end of input"
  ]


simpleTestSuite :: Test
simpleTestSuite = TestList
  [ parseTest "simpleTest011" simpleN3Graph_g1_01 g1  noError
  , parseTest "simpleTest012" simpleN3Graph_g1_02 g1  noError
  , parseTest "simpleTest012a" simpleN3Graph_g1_02a g1a  noError
  , parseTest "simpleTest013" simpleN3Graph_g1_03 g1  noError
  , parseTest "simpleTest014" simpleN3Graph_g1_04 g1  noError
  , parseTest "simpleTest015" simpleN3Graph_g1_05 g1b noError
  , parseTest "simpleTest016" simpleN3Graph_g1_06 emptyRDFGraph emsg16
  , parseTest "simpleTest03"  simpleN3Graph_g2    g2  noError
  , parseTest "simpleTest04"  simpleN3Graph_g3    g3  noError
  , parseTest "simpleTest05"  simpleN3Graph_g4    g4  noError
  , parseTest "simpleTest06"  simpleN3Graph_g5    g5  noError
  , parseTest "simpleTest07"  simpleN3Graph_g6    g6  noError
  , parseTest "simpleTest08"  simpleN3Graph_g7    g7  noError
  , parseTest "simpleTest09"  simpleN3Graph_g8    g8  noError
  , parseTest "simpleTest10"  simpleN3Graph_g81   g81 noError
    -- , simpleTest11  = parseTest "simpleTest11"  simpleN3Graph_g82   g82 noError
  , parseTest "simpleTest12"  simpleN3Graph_g83   g83 noError
  , parseTest "simpleTest13"  simpleN3Graph_g9    g9  noError
  , parseTest "simpleTest14"  simpleN3Graph_g10   g10 noError
  , parseTest "simpleTest15"  simpleN3Graph_g11   g11 noError
  , parseTest "simpleTest16"  simpleN3Graph_g12   g12 noError
  , parseTest "simpleTest17"  simpleN3Graph_g17   g17 noError
  ]

------------------------------------------------------------
--  Exotic parser tests
------------------------------------------------------------
--
--  These tests cover various forms of anonymous nodes
--  [...], lists and formula. together with uses of ':-'
--

--  Simple anon nodes, with semicolons and commas
exoticN3Graph_x1 :: String
exoticN3Graph_x1 =
    commonPrefixes ++
    " [ base1:p1 base1:o1 ; \n" ++
    "   base1:p1 base2:o2 ; \n" ++
    "   base2:p2 base2:o2 ; \n" ++
    "   base2:p2 base3:o3 ] = base1:s1 . \n" ++
    " base2:s2 = \n" ++
    " [ base1:p1 base1:o1 , \n" ++
    "   base2:o2 , \n" ++
    "   base3:o3 , \n" ++
    "   \"l1\"   ; \n" ++
    "   base2:p2 base1:o1 , \n" ++
    "            base2:o2 , \n" ++
    "            base3:o3 , \n" ++
    "            \"l1\"   ] . \n"

--  Simple anon nodes, with 'is ... of' and semicolons and commas
exoticN3Graph_x2 :: String
exoticN3Graph_x2 =
    commonPrefixes ++
    " [ @has base1:p1     base1:o1 ; \n" ++
    "   @is  base1:p1 @of base2:o2 ; \n" ++
    "   @has base2:p2     base2:o2 ; \n" ++
    "   @is  base2:p2 @of base3:o3 ] = base1:s1 . \n" ++
    " base2:s2 = \n" ++
    " [ @has base1:p1 base1:o1 , \n" ++
    "                 base2:o2 , \n" ++
    "                 base3:o3 , \n" ++
    "                 \"l1\"   ; \n" ++
    "   @is  base2:p2 @of base1:o1 , \n" ++
    "                     base2:o2 , \n" ++
    "                     base3:o3 , \n" ++
    "                     \"l1\"   ] . \n"


{-
--  Simple anon nodes, attached to identified node
exoticN3Graph_x3 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    " [ has base1:p1 of base1:o1 ; \n" ++
    "   is  base1:p1 of base2:o2 ; \n" ++
    "   has base2:p2 of base2:o2 ; \n" ++
    "   is  base2:p2 of base3:o3 ] . \n" ++
    " base2:s2 :- \n" ++
    " [ has base1:p1 of base1:o1 , \n" ++
    "                   base2:o2 , \n" ++
    "                   base3:o3 , \n" ++
    "                   \"l1\"   ; \n" ++
    "   is  base2:p2 of base1:o1 , \n" ++
    "                   base2:o2 , \n" ++
    "                   base3:o3 , \n" ++
    "                   \"l1\"   ] . \n"

-}

--  List nodes, with and without :-

exoticN3Graph_x4 :: String
exoticN3Graph_x4 =
    commonPrefixes ++
    " base1:s1 = (base1:o1 base2:o2 base3:o3 \"l1\") .\n"

exoticN3Graph_x5 :: String
exoticN3Graph_x5 =
    commonPrefixes ++
    " (base1:o1 base2:o2 base3:o3 \"l1\") = base1:s1 .\n"

{-
exoticN3Graph_x6 =
    commonPrefixes ++
    " base1:s1 :- (base1:o1 base2:o2 base3:o3 \"l1\") .\n"
-}

--  Formula nodes, with and without :-

exoticN3Graph_x7 :: String
exoticN3Graph_x7 =
    commonPrefixes ++
    " { base1:s1 base1:p1 base1:o1 .   \n" ++
    "   base2:s2 base1:p1 base2:o2 .   \n" ++
    "   base3:s3 base1:p1 base3:o3 . } \n" ++
    " base2:p2 base2:f2 . "

{-
exoticN3Graph_x8 =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    " { base1:s1 base1:p1 base1:o1 .     \n" ++
    "   base2:s2 base1:p1 base2:o2 .     \n" ++
    "   base3:s3 base1:p1 base3:o3 . } ; \n" ++
    " base2:p2 base2:f2 . "

exoticN3Graph_x9 =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    "   { base1:s1 base1:p1 base1:o1 } ; \n" ++
    "   base2:p2 base2:f2 "
    -- (also omits final periods)

exoticN3Graph_x8a =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    " { base1:s1 base1:p1 base1:o1 .     \n" ++
    "   base2:s2 base1:p1 base2:o2 .     \n" ++
    "   base3:s3 base1:p1 base3:o3 . } . \n" ++
    " base1:f1 base2:p2 base2:f2 . "

exoticN3Graph_x9a =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    " { base1:s1 base1:p1 base1:o1 . } . \n" ++
    " base1:f1 base2:p2 base2:f2 . "
-}

--  Test allocation of bnodes carries over a nested formula
exoticN3Graph_x12 :: String
exoticN3Graph_x12 =
    commonPrefixes ++
    " base1:s1 base1:p1 [ base1:p1 base1:o1 ] .     \n" ++
    " { base2:s2 base2:p2 [ base2:p2 base2:o2 ] . } \n" ++
    "            base2:p2 base2:f2 .                \n" ++
    " base3:s3 base3:p3 [ base3:p3 base3:o3 ] ."

{-
--  List of bnodes
exoticN3Graph_x13 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1] \n" ++
    "    [base1:p1 base2:o2] \n" ++
    "    [base1:p1 base3:o3] ) .\n"

--  List of more complex bnodes
exoticN3Graph_x14 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1; base2:p2 base1:o1] \n" ++
    "    [base1:p1 base2:o2; base2:p2 base2:o2] \n" ++
    "    [base1:p1 base3:o3; base2:p2 base3:o3] ) .\n"

--  List with nested list
exoticN3Graph_x15 :: String
exoticN3Graph_x15 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1] \n"++
    "    [base2:p2 \n" ++
    "       ( [base1:p1 base1:o1] \n" ++
    "         [base1:p1 base2:o2] \n" ++
    "         [base1:p1 base3:o3] ) ] \n"++
    "    [base1:p1 base3:o3] ) .\n"

--  More complex list with nested list
exoticN3Graph_x16 :: String
exoticN3Graph_x16 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1; base2:p2 base1:o1] \n"++
    "    [base2:p2 \n" ++
    "       ( [base1:p1 base1:o1; base2:p2 base1:o1] \n" ++
    "         [base1:p1 base2:o2; base2:p2 base2:o2] \n" ++
    "         [base1:p1 base3:o3; base2:p2 base3:o3] ) ] \n"++
    "    [base1:p1 base3:o3; base2:p2 base3:o3] ) .\n"
-}

exoticTestSuite :: Test
exoticTestSuite = TestList
  [ parseTest "exoticTest01" exoticN3Graph_x1  x1  noError
    , parseTest "exoticTest02" exoticN3Graph_x2  x2  noError
      -- , parseTest "exoticTest03" exoticN3Graph_x3  x3  noError
    , parseTest "exoticTest04" exoticN3Graph_x4  x4  noError
    , parseTest "exoticTest05" exoticN3Graph_x5  x5  noError
      -- , parseTest "exoticTest06" exoticN3Graph_x6  x6  noError
    , parseTest "exoticTest07" exoticN3Graph_x7  x7  noError
      -- , parseTest "exoticTest08" exoticN3Graph_x8  x8  noError
      -- , parseTest "exoticTest09" exoticN3Graph_x9  x9  noError
      -- , parseTest "exoticTest10" exoticN3Graph_x8a x8  noError
      -- , parseTest "exoticTest11" exoticN3Graph_x9a x9  noError
    , parseTest "exoticTest12" exoticN3Graph_x12 x12 noError
      -- , parseTest "exoticTest13" exoticN3Graph_x13 x13 noError
      -- , parseTest "exoticTest14" exoticN3Graph_x14 x14 noError
      -- , parseTest "exoticTest15" exoticN3Graph_x15 x15 noError
      -- , parseTest "exoticTest16" exoticN3Graph_x16 x16 noError
    , testGraphEq "exoticTest20" False x7 x8
    , testGraphEq "exoticTest21" False x8 x9
    
  ]

------------------------------------------------------------
--  Test parser failure
------------------------------------------------------------
--
--  Very limited at the moment.
--

failTest :: String -> String -> String -> Test
failTest lbl gr pe = parseTest lbl gr emptyRDFGraph pe

failN3Graph_g1 :: String
failN3Graph_g1 =
    commonPrefixes ++
    " base1:s1 base2:p2 unknown3:o3 . "

fail1 :: String
fail1 = intercalate "\n" [
         "",
         " base1:s1 base2:p2 unknown3:o3 . ",
         "                   ^",
         "(line 4, column 20 indicated by the '^' sign above):",
         "",
         "unexpected Prefix 'unknown3:' not bound.",
         "expecting pathitem"
        ]

failTestSuite :: Test
failTestSuite = TestList
  [ failTest "failTest01" failN3Graph_g1 fail1
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ charTestSuite
  , nameTestSuite
  , prefixTestSuite
  , absUriRefTestSuite
  , uriRef2TestSuite
  , simpleTestSuite
  , exoticTestSuite
  , failTestSuite
  ]

main :: IO ()
main = runTestTT allTests >> return ()

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
