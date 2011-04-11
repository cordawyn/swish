--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3FormatterTest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This Module defines test cases for module Parse parsing functions.
--
--------------------------------------------------------------------------------

module Main where

import Swish.RDF.N3Formatter
    ( formatGraphAsStringNl
    , formatGraphAsString
    , formatGraphDiag )

import Swish.RDF.N3Parser (parseN3fromString)

import Swish.RDF.RDFGraph
    ( RDFGraph, RDFLabel(..), NSGraph(..)
    , NamespaceMap
    , LookupFormula(..)
    , emptyRDFGraph, toRDFGraph
      -- Export selected RDFLabel values
    , res_rdf_type, res_rdf_first, res_rdf_rest, res_rdf_nil
    , res_owl_sameAs
    )

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    )

import Swish.Utils.LookupMap
    ( LookupMap(..)
    , emptyLookupMap, makeLookupMap )

import Swish.RDF.GraphClass (Arc, arc)

import Test.HUnit
    ( Test(TestCase,TestList)
    , assertEqual, runTestTT )

------------------------------------------------------------
--  Common test wrappers
------------------------------------------------------------

testLabelEq :: String -> Bool -> RDFLabel -> RDFLabel -> Test
testLabelEq lab eq n1 n2 =
    TestCase ( assertEqual ("testLabelEq:"++lab) eq (n1==n2) )

testGraphEq :: String -> Bool -> RDFGraph -> RDFGraph -> Test
testGraphEq lab eq gg1 gg2 =
    TestCase ( assertEqual ("testGraphEq:"++lab) eq (gg1==gg2) )

------------------------------------------------------------
--  Define some common values
------------------------------------------------------------

base1, base2, base3, base4 :: Namespace
base1 = Namespace "base1" "http://id.ninebynine.org/wip/2003/test/graph1/node#"
base2 = Namespace "base2" "http://id.ninebynine.org/wip/2003/test/graph2/node/"
base3 = Namespace "base3" "http://id.ninebynine.org/wip/2003/test/graph3/node"
base4 = Namespace "base4" "http://id.ninebynine.org/wip/2003/test/graph3/nodebase"

s1, s2, s3 :: RDFLabel
s1 = Res $ ScopedName base1 "s1"
s2 = Res $ ScopedName base2 "s2"
s3 = Res $ ScopedName base3 "s3"

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

p1, p2, p3, p21, p22, p23, p24, p25, p26 :: RDFLabel
p1  = Res $ ScopedName base1 "p1"
p2  = Res $ ScopedName base2 "p2"
p3  = Res $ ScopedName base3 "p3"
p21 = Res $ ScopedName base2 "p21"
p22 = Res $ ScopedName base2 "p22"
p23 = Res $ ScopedName base2 "p23"
p24 = Res $ ScopedName base2 "p24"
p25 = Res $ ScopedName base2 "p25"
p26 = Res $ ScopedName base2 "p26"

o1, o2, o3 :: RDFLabel
o1 = Res $ ScopedName base1 "o1"
o2 = Res $ ScopedName base2 "o2"
o3 = Res $ ScopedName base3 "o3"

l1txt, l2txt, l3txt, l11txt, l12txt, l13txt, l14txt :: String
l1txt = "l1"
l2txt = "l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'"
l3txt = "l3--\r\"'\\--\x0020\&--\x00A0\&--"
l11txt = "lx11"
l12txt = "lx12"
l13txt = "lx13"
l14txt = "lx14"

l1, l2, l3, l11, l12, l13, l14 :: RDFLabel
l1  = Lit l1txt  Nothing
l2  = Lit l2txt  Nothing
l3  = Lit l3txt  Nothing
l11 = Lit l11txt Nothing
l12 = Lit l12txt Nothing
l13 = Lit l13txt Nothing
l14 = Lit l14txt Nothing

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

t01, t02, t03, t04, t05, t06, t07 :: Arc RDFLabel
t01 = arc s1 p1 o1
t02 = arc s2 p1 o2
t03 = arc s3 p1 o3
t04 = arc s1 p1 l1
t05 = arc s2 p1 b1
t06 = arc s3 p1 l2
t07 = arc s3 p2 l3

nslist :: NamespaceMap
nslist = makeLookupMap
    [ base1
    , base2
    , base3
    , base4
    ]

g1np :: RDFGraph
g1np = toRDFGraph [t01]

toGraph :: [Arc RDFLabel] -> RDFGraph
toGraph arcs = (toRDFGraph arcs) {namespaces = nslist}

g1, g1b1, g1b3, g1a1, g1l1, g1l2 :: RDFGraph
g1   = toGraph [t01]
g1b1 = toGraph [arc b1 p1 o1]
g1b3 = toGraph [arc b1 b2 b3]
g1a1 = toGraph [arc (Blank "1") p1 o1]
g1l1 = toGraph [arc s1 p1 l1]
g1l2 = toGraph [arc s1 p1 l2]

{-
g1f1 = NSGraph
        { namespaces = nslist
        , formulae   = formo1g1
        , statements = [f01]
        }
    where
        f01      = arc s1 p1 o1
        formo1g1 = LookupMap [Formula o1 g1]
-}

g1f2, g1f3 :: RDFGraph
g1f2 = NSGraph
        { namespaces = nslist
        , formulae   = formb2g1
        , statements = [f02]
        }
    where
        f02 = arc s1 p1 b2
        formb2g1 = LookupMap [Formula b2 g1]

g1f3 = NSGraph
        { namespaces = nslist
        , formulae   = formb3g1f2
        , statements = [f02]
        }
    where
        f02 = arc s1 p1 b3
        formb3g1f2 = LookupMap [Formula b3 g1f2]

----

g2, g3, g4, g5, g6, g7 :: RDFGraph
g2 = toGraph [t01,t02,t03]
g3 = toGraph [t01,t04]
g4 = toGraph [t01,t05]
g5 = toGraph [t01,t02,t03,t04,t05]
g6 = toGraph [t01,t06]
g7 = toGraph [t01,t07]

t801, t802, t807, t808, t809, t810 :: Arc RDFLabel
t801 = arc s1 res_rdf_type       o1
t802 = arc s2 res_owl_sameAs     o2
t807 = arc o1 p1 s1
t808 = arc s2 p1 o2
t809 = arc s1 p2 o1
t810 = arc o2 p2 s2

g8, g81, g83 :: RDFGraph
g8  = toGraph [t801,t802,t807,t808,t809,t810]
g81 = toGraph [t801,t802]
g83 = toGraph [t807,t808,t809,t810]

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
g9 = toGraph [t911,t912,t913,t914,
              t921,t922,t923,t924,
              t925,t926,t927,t928]

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
-- t1028 = arc l1 p2 s2
t1028 = arc s1 p2 s2

g10 :: RDFGraph
g10 = toGraph [t1011,t1012,t1013,t1014,
               t1021,t1022,t1023,t1024,
               t1025,t1026,t1027,t1028]

g11 :: RDFGraph
g11 = toGraph [ arc s1 p1 v1
              , arc v2 p1 o1
              , arc v3 p1 v4]

tx101, tx102, tx111, tx112, tx113, tx114,
  tx121, tx122, tx123, tx124, tx125, tx126,
  tx127, tx128 :: Arc RDFLabel
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
tx128 = arc b2 p2 l2

x1 :: RDFGraph
x1 = toGraph [tx101,tx102,
              tx111,tx112,tx113,tx114,
              tx121,tx122,tx123,tx124,
              tx125,tx126,tx127,tx128]

tx201, tx202, tx211, tx212, tx213, tx214,
  tx221, tx222, tx223, tx224, tx225, tx226,
  tx227 :: Arc RDFLabel
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
-- tx228 = arc l1 p2 b2

x2 :: RDFGraph
x2 = toGraph [tx201,tx202,
              tx211,tx212,tx213,tx214,
              tx221,tx222,tx223,tx224,
              tx225,tx226,tx227]

tx311, tx312, tx313, tx314,
  tx321, tx322, tx323, tx324, tx325, tx326,
  tx327 :: Arc RDFLabel
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
-- tx328 = arc l1 p2 s2

x3 :: RDFGraph
x3 = toGraph [tx311,tx312,tx313,tx314,
              tx321,tx322,tx323,tx324,
              tx325,tx326,tx327]

tx401, tx402, tx403, tx404, tx405, tx406,
  tx407, tx408, tx409 :: Arc RDFLabel
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
x4 = toGraph [tx401,tx402,tx403,tx404,
              tx405,tx406,tx407,tx408,
              tx409]

x5 :: RDFGraph
x5 = toGraph [ arc b1 res_owl_sameAs s1
             , arc b1 res_rdf_first  o1
             , arc b1 res_rdf_rest   b2
             , arc b2 res_rdf_first  o2
             , arc b2 res_rdf_rest   b3
             , arc b3 res_rdf_first  o3
             , arc b3 res_rdf_rest   b4
             , arc b4 res_rdf_first  l1
             , arc b4 res_rdf_rest   res_rdf_nil
             ]

{-
I was aiming for

:s1     =  (
        :o1
        b2:o2
        b3:o3
        "l1" ) .

but really it's

:s1 rdf:first ( b1:o1 b2:o2 b3:o3 "l1" ) .

or something like that. different versions of
cwm parse the triples differently, and it depends
on the output format too (eg n3 vs ntriples).
-}

x6 :: RDFGraph
x6 = toGraph [ arc s1 res_rdf_first o1
             , arc s1 res_rdf_rest  b2
             , arc b2 res_rdf_first o2
             , arc b2 res_rdf_rest  b3
             , arc b3 res_rdf_first o3
             , arc b3 res_rdf_rest  b4
             , arc b4 res_rdf_first l1
             , arc b4 res_rdf_rest  res_rdf_nil
             ]

x7 :: RDFGraph
x7 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b1 g2]
        , statements = [arc b1 p2 f2]
        }

x8 :: RDFGraph
x8 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula f1 g2]
        , statements = [arc f1 p2 f2]
        }

x9 :: RDFGraph
x9 = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula f1 g1]
        , statements = [arc f1 p2 f2]
        }
        
--  Test allocation of bnodes carries over a nested formula

x12, x12fg :: RDFGraph
x12    = NSGraph
        { namespaces = nslist
        , formulae   = LookupMap [Formula b2 x12fg]
        , statements = [ arc s1 p1 b1
                       , arc b1 p1 o1
                       , arc b2 p2 f2
                       , arc s3 p3 b3
                       , arc b3 p3 o3
                       ]
        }

x12fg  = toRDFGraph [ arc s2 p2 b4
                    , arc b4 p2 o2
                    ]
        
--  List of simple anon nodes

x13 :: RDFGraph
x13 = toGraph [ arc s1 res_rdf_first b1
              , arc s1 res_rdf_rest  c1
              , arc c1 res_rdf_first b2
              , arc c1 res_rdf_rest  c2
              , arc c2 res_rdf_first b3
              , arc c2 res_rdf_rest  res_rdf_nil
              , arc b1 p1 o1
              , arc b2 p1 o2
              , arc b3 p1 o3
              ]

--  List of simple anon nodes using autogenerated bnodes

x13a :: RDFGraph
x13a = toGraph [ arc s1  res_rdf_first b_1
               , arc s1  res_rdf_rest  c_1
               , arc c_1 res_rdf_first b_2
               , arc c_1 res_rdf_rest  c_2
               , arc c_2 res_rdf_first b_3
               , arc c_2 res_rdf_rest  res_rdf_nil
               , arc b_1 p1 o1
               , arc b_2 p1 o2
               , arc b_3 p1 o3
               ]
  where
    b_1 = Blank "1"
    b_2 = Blank "2"
    b_3 = Blank "3"
    c_1 = Blank "4"
    c_2 = Blank "5"

--  List of more complex anon nodes

x14 :: RDFGraph
x14 = toGraph [ arc s1 res_rdf_first b1
              , arc s1 res_rdf_rest  c1
              , arc c1 res_rdf_first b2
              , arc c1 res_rdf_rest  c2
              , arc c2 res_rdf_first b3
              , arc c2 res_rdf_rest  res_rdf_nil
              , arc b1 p1 o1
              , arc b1 p2 o1
              , arc b2 p1 o2
              , arc b2 p2 o2
              , arc b3 p1 o3
              , arc b3 p2 o3
              ]

--  List with nested list

x15 :: RDFGraph
x15 = toGraph [ arc s1 res_rdf_first b1
              , arc s1 res_rdf_rest  c1
              , arc c1 res_rdf_first b2
              , arc c1 res_rdf_rest  c2
              , arc c2 res_rdf_first b3
              , arc c2 res_rdf_rest  res_rdf_nil
              , arc b1 p1 o1
              , arc b2 p2 c3
              , arc b3 p1 o3

              , arc c3 res_rdf_first b4
              , arc c3 res_rdf_rest  c4
              , arc c4 res_rdf_first b5
              , arc c4 res_rdf_rest  c5
              , arc c5 res_rdf_first b6
              , arc c5 res_rdf_rest  res_rdf_nil
              , arc b4 p1 o1
              , arc b5 p1 o2
              , arc b6 p1 o3
              ]

--  More complex list with nested list

x16 :: RDFGraph
x16 = toGraph [ arc s1 res_rdf_first b1
              , arc s1 res_rdf_rest  c1
              , arc c1 res_rdf_first b2
              , arc c1 res_rdf_rest  c2
              , arc c2 res_rdf_first b3
              , arc c2 res_rdf_rest  res_rdf_nil
              , arc b1 p1 o1
              , arc b1 p2 o1
              , arc b2 p2 c3
              , arc b3 p1 o3
              , arc b3 p2 o3

              , arc c3 res_rdf_first b4
              , arc c3 res_rdf_rest  c4
              , arc c4 res_rdf_first b5
              , arc c4 res_rdf_rest  c5
              , arc c5 res_rdf_first b6
              , arc c5 res_rdf_rest  res_rdf_nil
              , arc b4 p1 o1
              , arc b4 p2 o1
              , arc b5 p1 o2
              , arc b5 p2 o2
              , arc b6 p1 o3
              , arc b6 p2 o3
              ]

--  Troublesome example

x17 :: RDFGraph
x17 = toGraph [ arc s1 res_rdf_type  o1
              , arc s1 res_rdf_first b1
              , arc s1 res_rdf_rest  c1
              , arc c1 res_rdf_first b2
              , arc c1 res_rdf_rest  res_rdf_nil

              , arc b1 p21 o2
              , arc b1 p22 c2

              , arc b2 p24 o3
              , arc b2 p25 l13

              , arc c2 res_rdf_first b3
              , arc c2 res_rdf_rest  c3
              , arc c3 res_rdf_first l12
              , arc c3 res_rdf_rest  res_rdf_nil

              , arc b3 p23 l11
              ]

-- collection graphs

graph_c1, graph_c1rev, graph_c2, graph_c2rev,
  graph_c3 :: RDFGraph
graph_c1    = toGraph [arc s1 p1 res_rdf_nil]
graph_c1rev = toGraph [arc res_rdf_nil p1 o1]
graph_c2    = toGraph [arc s1 p1 b1,
                       arc b1 res_rdf_first l1,
                       arc b1 res_rdf_rest b2,
                       arc b2 res_rdf_first o2,
                       arc b2 res_rdf_rest b3,
                       arc b3 res_rdf_first l2,
                       arc b3 res_rdf_rest b4,
                       arc b4 res_rdf_first o3,
                       arc b4 res_rdf_rest res_rdf_nil]
graph_c2rev = toGraph [arc b1 res_rdf_first l1,
                       arc b1 res_rdf_rest b2,
                       arc b2 res_rdf_first o2,
                       arc b2 res_rdf_rest b3,
                       arc b3 res_rdf_first l2,
                       arc b3 res_rdf_rest b4,
                       arc b4 res_rdf_first o3,
                       arc b4 res_rdf_rest res_rdf_nil,
                       arc b1 p1 o1]
graph_c3    = toGraph [arc s1 p1 b1,
                       arc b1 res_rdf_first l1,
                       arc b1 res_rdf_rest b2,
                       arc b2 res_rdf_first o2,
                       arc b2 res_rdf_rest b3,
                       arc b3 res_rdf_first l2,
                       arc b3 res_rdf_rest b4,
                       arc b4 res_rdf_first o3,
                       arc b4 res_rdf_rest res_rdf_nil,
                       arc s1 p2 res_rdf_nil,
                       arc s2 p2 o2]

-- bnode graphs

graph_b1, graph_b1rev, graph_b2, graph_b2rev, graph_b3 :: RDFGraph
graph_b1    = toGraph [arc s1 p1 b1]
graph_b1rev = toGraph [arc b1 p1 o1]
graph_b2    = toGraph [arc s1 p1 b1,
                       arc b1 p2 l1,
                       arc b1 o2 o3]
graph_b2rev = toGraph [arc b1 p2 l1,
                       arc b1 o2 o3,
                       arc b1 p1 o1]
graph_b3    = toGraph [arc s1 p1 b1,
                       arc b1 p2 l2,
                       arc b1 o2 o3,
                       arc s1 p2 b2,
                       arc s2 p2 o2]

------------------------------------------------------------
--  Trivial formatter tests
------------------------------------------------------------
--
--  These are very basic tests that confirm that output for a
--  simple graph corresponds exactly to some supplied string.

formatTest :: String -> RDFGraph -> String -> Test
formatTest lab gr out =
    TestList
      [ TestCase ( assertEqual ("formatTest:"++lab) out res )
      ]
    where
      res = formatGraphAsStringNl gr

diagTest :: String -> RDFGraph -> String -> Test
diagTest lab gr out =
    TestList
      [ TestCase ( assertEqual ("diag:text:"++lab) out (res "") )
      , TestCase ( assertEqual ("diag:map:"++lab) emptyLookupMap nmap )
      , TestCase ( assertEqual ("diag:gen:"++lab) 0 ngen )
      , TestCase ( assertEqual ("diag:trc:"++lab) [] trc )
      ]
    where
      (res,nmap,ngen,trc) = formatGraphDiag gr

commonPrefixes :: String
commonPrefixes =
    "@prefix base1: <" ++ nsURI base1 ++ "> .\n" ++
    "@prefix base2: <" ++ nsURI base2 ++ "> .\n" ++
    "@prefix base3: <" ++ nsURI base3 ++ "> .\n" ++
    "@prefix base4: <" ++ nsURI base4 ++ "> .\n"

--  Single statement using <uri> form
simpleN3Graph_g1_01 :: String
simpleN3Graph_g1_01 =
    "<http://id.ninebynine.org/wip/2003/test/graph1/node#s1> " ++
    "<http://id.ninebynine.org/wip/2003/test/graph1/node#p1> " ++
    "<http://id.ninebynine.org/wip/2003/test/graph1/node#o1> .\n"

--  Single statement using prefix:name form
simpleN3Graph_g1_02 :: String
simpleN3Graph_g1_02 =
    commonPrefixes ++
    "base1:s1 base1:p1 base1:o1 .\n"

--  Single blank node
simpleN3Graph_g1_03 :: String
simpleN3Graph_g1_03 =
    commonPrefixes ++
    "[\n base1:p1 base1:o1\n] .\n"
    -- "_:b1 base1:p1 base1:o1 .\n"

--  Single auto-allocated blank node
simpleN3Graph_g1_04 :: String
simpleN3Graph_g1_04 =
    commonPrefixes ++
    "[\n base1:p1 base1:o1\n] .\n"
    -- "_:_1 base1:p1 base1:o1 .\n"

--  Single literal object
simpleN3Graph_g1_05 :: String
simpleN3Graph_g1_05 =
    commonPrefixes ++
    "base1:s1 base1:p1 \"l1\" .\n"

--  Single multiline literal object
simpleN3Graph_g1_06 :: String
simpleN3Graph_g1_06 =
    commonPrefixes ++
    "base1:s1 base1:p1 \"l2-'\\\"line1\\\"'\\n\\nl2-'\\\"\\\"line2\\\"\\\"'\" .\n"

-- this 'round trips' into a triple-quoted string
simpleN3Graph_g1_06_rt :: String
simpleN3Graph_g1_06_rt =
    commonPrefixes ++
    "base1:s1 base1:p1 \"\"\"l2-'\"line1\"'\n\nl2-'\"\"line2\"\"'\"\"\" .\n"

{-
--  Single statement with formula node
simpleN3Graph_g1_07 =
    commonPrefixes ++
    "base1:s1 base1:p1 base1:o1 .\n"++
    "base1:o1 :-\n"++
    "    {\n"++
    "    base1:s1 base1:p1 base1:o1\n"++
    "    } .\n"

-}

--  Single statement with formula blank node
simpleN3Graph_g1_08 :: String
simpleN3Graph_g1_08 =
    commonPrefixes ++
    "base1:s1 base1:p1  { \n"++
    "    base1:s1 base1:p1 base1:o1\n"++
    " }  .\n"
    
--  Three blank nodes (or is that blind mice?)
simpleN3Graph_g1_09 :: String
simpleN3Graph_g1_09 =
    commonPrefixes ++
    "[\n _:b2 []\n] .\n"
    -- "_:b1 _:b2 _:b3 .\n"

--  Simple nested formula case
simpleN3Graph_g1_10 :: String
simpleN3Graph_g1_10 =
    commonPrefixes ++
    "base1:s1 base1:p1  { \n"           ++
    "    base1:s1 base1:p1  { \n"       ++
    "        base1:s1 base1:p1 base1:o1\n" ++
    "     } \n"                          ++
    " }  .\n"

{-
Simple troublesome case
-}
    
simpleN3Graph_x13a :: String
simpleN3Graph_x13a =
    commonPrefixes ++
    "base1:s1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> " ++ b1s ++ " ;\n"++
    "         <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> ( " ++ b2s ++ " " ++ b3s ++ " ) .\n"
    where
      b1s = "[\n base1:p1 base1:o1\n]"
      b2s = "[\n base1:p1 base2:o2\n]"
      b3s = "[\n base1:p1 base3:o3\n]"

{-
Simple collection tests; may replicate some of the
previous tests.
-}

simpleN3Graph_c1 :: String
simpleN3Graph_c1 =
    commonPrefixes ++
    "base1:s1 base1:p1 () .\n"

simpleN3Graph_c1rev :: String
simpleN3Graph_c1rev =
    commonPrefixes ++
    "() base1:p1 base1:o1 .\n"

collItems :: String
collItems = "( \"l1\" base2:o2 \"\"\"" ++ l2txt ++ "\"\"\" base3:o3 )"

simpleN3Graph_c2 :: String
simpleN3Graph_c2 =
    commonPrefixes ++
    "base1:s1 base1:p1 " ++ collItems ++ " .\n"

simpleN3Graph_c2rev :: String
simpleN3Graph_c2rev =
    commonPrefixes ++
    collItems ++ " base1:p1 base1:o1 .\n"

simpleN3Graph_c3 :: String
simpleN3Graph_c3 =
    commonPrefixes ++
    "base1:s1 base1:p1 " ++ collItems ++ " ;\n" ++
    "         base2:p2 () .\n" ++
    "base2:s2 base2:p2 base2:o2 .\n"

{-
Simple bnode tests; may replicate some of the
previous tests.
-}

simpleN3Graph_b1 :: String
simpleN3Graph_b1 =
    commonPrefixes ++
    "base1:s1 base1:p1 [] .\n"

simpleN3Graph_b1rev :: String
simpleN3Graph_b1rev =
    commonPrefixes ++
    "[\n base1:p1 base1:o1\n] .\n"

simpleN3Graph_b2 :: String
simpleN3Graph_b2 =
    commonPrefixes ++
    "base1:s1 base1:p1 [\n base2:o2 base3:o3 ;\n base2:p2 \"l1\"\n] .\n"

simpleN3Graph_b2rev :: String
simpleN3Graph_b2rev =
    commonPrefixes ++
    "[\n base1:p1 base1:o1 ;\n base2:o2 base3:o3 ;\n base2:p2 \"l1\"\n] .\n"

simpleN3Graph_b3 :: String
simpleN3Graph_b3 =
    commonPrefixes ++
    "base1:s1 base1:p1 [\n base2:o2 base3:o3 ;\n base2:p2 \"\"\"" ++ l2txt ++ "\"\"\"\n] ;\n" ++
    "         base2:p2 [] .\n" ++
    "base2:s2 base2:p2 base2:o2 .\n"

-- diag13 = diagTest "trivialTest13" x13a simpleN3Graph_x13a

trivialTestSuite :: Test
trivialTestSuite = TestList
 [ formatTest "trivialTest01" g1np simpleN3Graph_g1_01
 , formatTest "trivialTest02" g1   simpleN3Graph_g1_02
 , formatTest "trivialTest03" g1b1 simpleN3Graph_g1_03
 , formatTest "trivialTest04" g1a1 simpleN3Graph_g1_04
 , formatTest "trivialTest05" g1l1 simpleN3Graph_g1_05
 , formatTest "trivialTest06" g1l2 simpleN3Graph_g1_06_rt
   -- trivialTest07 = formatTest "trivialTest07" g1f1 simpleN3Graph_g1_07 -- formula is a named node
 , formatTest "trivialTest08" g1f2 simpleN3Graph_g1_08
 , formatTest "trivialTest09" g1b3 simpleN3Graph_g1_09
 , formatTest "trivialTest10" g1f3 simpleN3Graph_g1_10
 , formatTest "trivialTest13a" x13a simpleN3Graph_x13a

 , formatTest "trivialTestc1" graph_c1 simpleN3Graph_c1
 , formatTest "trivialTestc2" graph_c2 simpleN3Graph_c2
 , formatTest "trivialTestc3" graph_c3 simpleN3Graph_c3
 , formatTest "trivialTestc1rev" graph_c1rev simpleN3Graph_c1rev
 , formatTest "trivialTestc2rev" graph_c2rev simpleN3Graph_c2rev

 , formatTest "trivialTestb1" graph_b1 simpleN3Graph_b1
 , formatTest "trivialTestb2" graph_b2 simpleN3Graph_b2
 , formatTest "trivialTestb3" graph_b3 simpleN3Graph_b3
 , formatTest "trivialTestb1rev" graph_b1rev simpleN3Graph_b1rev
 , formatTest "trivialTestb2rev" graph_b2rev simpleN3Graph_b2rev

 , formatTest "trivialTestx4" x4 exoticN3Graph_x4
 , formatTest "trivialTestx5" x5 exoticN3Graph_x5
 , formatTest "trivialTestx7" x7 exoticN3Graph_x7
   
 ]

------------------------------------------------------------
--  Parser tests to cross-check round-trip testing
------------------------------------------------------------

parseTest :: String -> String -> RDFGraph -> String -> Test
parseTest lab inp gr er =
    TestList
      [ TestCase ( assertEqual ("parseTestError:"++lab) er pe )
      , TestCase ( assertEqual ("parseTestGraph:"++lab) gr pg )
      ]
    where
        (pe,pg) = case parseN3fromString inp of
            Right g -> ("",g)
            Left  s -> (s,emptyRDFGraph)

noError, errorText :: String
noError   = ""
errorText = "*"

parseTestSuite :: Test
parseTestSuite = TestList
  [ parseTest "01" simpleN3Graph_g1_01 g1np noError
  , parseTest "02" simpleN3Graph_g1_02 g1   noError
  , parseTest "03" simpleN3Graph_g1_03 g1b1 noError
  , parseTest "04" simpleN3Graph_g1_04 g1a1 noError
  , parseTest "05" simpleN3Graph_g1_05 g1l1 noError
  , parseTest "06" simpleN3Graph_g1_06 g1l2 noError
  , parseTest "06rt" simpleN3Graph_g1_06_rt g1l2 noError
    -- parseTest07 = parseTest "07" simpleN3Graph_g1_07 g1f1 noError -- formula is a named node
  , parseTest "08" simpleN3Graph_g1_08 g1f2 noError
  ]

------------------------------------------------------------
--  Repeat above tests using parser and graph-comparison
------------------------------------------------------------
--
--  This establishes a framework that will be used for
--  more complex tests that are less susceptible to trivial
--  formatting differences.  The idea is to generate output
--  that can be parsed to obtain an equivalent graph.

roundTripTest :: String -> RDFGraph -> Test
roundTripTest lab gr =
    TestList
      [ TestCase ( assertEqual ("RoundTrip:gr:"++lab) gr pg )
      , TestCase ( assertEqual ("RoundTrip:er:"++lab) "" pe )
      -- , TestCase ( assertEqual ("Formatted:"++lab) "" out )
      ]
    where
        out     = formatGraphAsString gr
        (pe,pg) = case parseN3fromString out of
            Right g -> ("",g)
            Left  s -> (s,emptyRDFGraph)

--  Full round trip from graph source.  This test may pick up some errors
--  the bnode generation logic that are not tested by hand-assembled graph
--  data structures.
fullRoundTripTest :: String -> String -> Test
fullRoundTripTest lab grstr =
    TestList
      [ TestCase ( assertEqual ("FullRoundTrip:gr:"++lab) gr pg )
      , TestCase ( assertEqual ("FullRoundTrip:er:"++lab) "" pe )
      -- , TestCase ( assertEqual ("FullRoundTrip:"++lab) "" out )
      ]
    where
        (_,gr) = case parseN3fromString grstr of
            Right g -> ("",g)
            Left  s -> (s,emptyRDFGraph)
        out     = formatGraphAsString gr
        (pe,pg) = case parseN3fromString out of
            Right g -> ("",g)
            Left  s -> (s,emptyRDFGraph)

roundTripTestSuite :: Test
roundTripTestSuite = TestList
  [ roundTripTest "01" g1np
  , roundTripTest "02" g1
  , roundTripTest "03" g1b1
  , roundTripTest "04" g1a1
  , roundTripTest "05" g1l1
  , roundTripTest "06" g1l2
    -- roundTripTest07 = roundTripTest "07" g1f1 -- formula is a named node
  , roundTripTest "08" g1f2
  , fullRoundTripTest "11" simpleN3Graph_g1_01
  , fullRoundTripTest "12" simpleN3Graph_g1_02
  , fullRoundTripTest "13" simpleN3Graph_g1_03
  , fullRoundTripTest "14" simpleN3Graph_g1_04
  , fullRoundTripTest "15" simpleN3Graph_g1_05
  , fullRoundTripTest "16rt" simpleN3Graph_g1_06_rt
    -- roundTripTest17 = fullRoundTripTest "17" simpleN3Graph_g1_07 -- TODO: :- with named node for formula
  , fullRoundTripTest "18" simpleN3Graph_g1_08
  ]

------------------------------------------------------------
--  Simple formatter tests
------------------------------------------------------------
--
--  These are simple tests that format and re-parse a graph,
--  and make sure that the result graph compares the same as
--  the original.  Therefore, depends on a trusted parser and
--  graph compare function.

simpleTest :: String -> RDFGraph -> Test
simpleTest lab = roundTripTest ("SimpleTest:"++lab)

simpleTestSuite :: Test
simpleTestSuite = TestList
  [ simpleTest "01" g2
  , simpleTest "02" g3
  , simpleTest "03" g4
  , simpleTest "04" g5
  , simpleTest "05" g6
  , simpleTest "06" g7
  , simpleTest "07" g8
  , simpleTest "08" g81
  , simpleTest "10" g83
  , simpleTest "11" g9
  , simpleTest "12" g10
  , simpleTest "13" g11
  ]

------------------------------------------------------------
--  Exotic parser tests
------------------------------------------------------------
--
--  These tests cover various forms of anonymous nodes
--  [...], lists and formulae.
--

-- does a round-trip test starting with the
exoticTest :: String -> RDFGraph -> Test
exoticTest lab gr =
    TestList
      [ TestCase ( assertEqual ("ExoticTest:gr:"++lab) gr pg )
      , TestCase ( assertEqual ("ExoticTest:er:"++lab) "" pe )
      -- , TestCase ( assertEqual ("ExoticTest:"++lab)    "" out )
      ]
    where
        out     = formatGraphAsString gr
        (pe,pg) = case parseN3fromString out of
            Right g -> ("",g)
            Left  s -> (s,emptyRDFGraph)

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
    "            \"\"\"" ++ l2txt ++ "\"\"\"   ] . \n"

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
    "                     base3:o3 ] . \n"
    -- "                   \"l1\"   ] . \n"

--  Simple anon nodes, attached to identified node
{-
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
    "                   base3:o3 ] . \n"
    -- "                   \"l1\"   ] . \n"
-}

--  List nodes, with and without :-

exoticN3Graph_x4 :: String
exoticN3Graph_x4 =
    commonPrefixes ++
    "base1:s1 = ( base1:o1 base2:o2 base3:o3 \"l1\" ) .\n"

exoticN3Graph_x5 :: String
exoticN3Graph_x5 =
    commonPrefixes ++
    "( base1:o1 base2:o2 base3:o3 \"l1\" ) = base1:s1 .\n"

{-
exoticN3Graph_x6 =
    commonPrefixes ++
    " base1:s1 :- (base1:o1 base2:o2 base3:o3 \"l1\") .\n"
-}

--  Formula nodes

exoticN3Graph_x7 :: String
exoticN3Graph_x7 =
    commonPrefixes ++
    " { \n" ++
    "    base1:s1 base1:p1 base1:o1 .\n" ++
    "    base2:s2 base1:p1 base2:o2 .\n" ++
    "    base3:s3 base1:p1 base3:o3\n" ++
    " }  base2:p2 base2:f2 .\n"

-- as above with the trailing . in the formula
exoticN3Graph_x7a :: String
exoticN3Graph_x7a =
    commonPrefixes ++
    " { \n" ++
    "    base1:s1 base1:p1 base1:o1 .\n" ++
    "    base2:s2 base1:p1 base2:o2 .\n" ++
    "    base3:s3 base1:p1 base3:o3 .\n" ++
    " }  base2:p2 base2:f2 ."

{-
exoticN3Graph_x8 =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    " { base1:s1 base1:p1 base1:o1 .     \n" ++
    "   base2:s2 base1:p1 base2:o2 .     \n" ++
    "   base3:s3 base1:p1 base3:o3 . } ; \n" ++
    " base2:p2 base2:f2 . "
-}

{-
exoticN3Graph_x9 =
    commonPrefixes ++
    " base1:f1 :- \n" ++
    " { base1:s1 base1:p1 base1:o1 . } ; \n" ++
    " base2:p2 base2:f2 . "
-}

--  Test allocation of bnodes over a nested formula
exoticN3Graph_x12 :: String
exoticN3Graph_x12 =
    commonPrefixes ++
    " base1:s1 base1:p1 [ base1:p1 base1:o1 ] .     \n" ++
    " { base2:s2 base2:p2 [ base2:p2 base2:o2 ] . } \n" ++
    "            base2:p2 base2:f2 .                \n" ++
    " base3:s3 base3:p3 [ base3:p3 base3:o3 ] ."

--  List of bnodes
{-
exoticN3Graph_x13 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1] \n" ++
    "    [base1:p1 base2:o2] \n" ++
    "    [base1:p1 base3:o3] ) .\n"
-}

{-
TODO
Hmm, what does the input graph really mean?

can we test the following somewhere (do we already?)
exoticN3Graph_x13 =
    commonPrefixes ++
    " base1:s1 = \n" ++
    "  ( [base1:p1 base1:o1] \n" ++
    "    [base1:p1 base2:o2] \n" ++
    "    [base1:p1 base3:o3] ) .\n"
-}

--  List of more complex bnodes
{-
exoticN3Graph_x14 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1; base2:p2 base1:o1] \n" ++
    "    [base1:p1 base2:o2; base2:p2 base2:o2] \n" ++
    "    [base1:p1 base3:o3; base2:p2 base3:o3] ) .\n"
-}
exoticN3Graph_x14 :: String
exoticN3Graph_x14 =
    commonPrefixes ++
    " base1:s1 = \n" ++
    "  ( [base1:p1 base1:o1; base2:p2 base1:o1] \n" ++
    "    [base1:p1 base2:o2; base2:p2 base2:o2] \n" ++
    "    [base1:p1 base3:o3; base2:p2 base3:o3] ) .\n"

--  List with nested list
{-
exoticN3Graph_x15 =
    commonPrefixes ++
    " base1:s1 :- \n" ++
    "  ( [base1:p1 base1:o1] \n"++
    "    [base2:p2 \n" ++
    "       ( [base1:p1 base1:o1] \n" ++
    "         [base1:p1 base2:o2] \n" ++
    "         [base1:p1 base3:o3] ) ] \n"++
    "    [base1:p1 base3:o3] ) .\n"
-}

--  More complex list with nested list
{-
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

--  Troublesome example
{-
exoticN3Graph_x17 =
    commonPrefixes ++
    "base1:s1 a base1:o1 ; :- \n" ++
    "  ( [ base2:p21 base2:o2  ;  \n" ++
    "      base2:p22 ( [ base2:p23 \"lx11\" ] \"lx12\" ) ] \n" ++
    "    [ base2:p24 base3:o3 ; base2:p25 \"lx13\" ] \n" ++
    "  ) . \n"
-}

--  Null prefixes
{-
exoticN3Graph_x18 =
    commonPrefixes ++
    "@prefix : <#> . " ++
    ":s1 a :o1 ; :- \n" ++
    "  ( [ :p21 :o2  ;  \n" ++
    "      :p22 ( [ :p23 \"lx11\" ] \"lx12\" ) ] \n" ++
    "    [ :p24 :o3 ; :p25 \"lx13\" ] \n" ++
    "  ) . \n"
-}

-- Check graph sources parse to expected values

exoticTestSuite :: Test
exoticTestSuite = TestList
  [ parseTest "exoticParseTest01" exoticN3Graph_x1 x1 noError
  , parseTest "exoticParseTest02" exoticN3Graph_x2 x2 noError
    -- exoticParseTest03 = parseTest "exoticParseTest03" exoticN3Graph_x3 x3 noError
  , parseTest "exoticParseTest04" exoticN3Graph_x4 x4 noError
  , parseTest "exoticParseTest05" exoticN3Graph_x5 x5 noError
    -- exoticParseTest06 = parseTest "exoticParseTest06" exoticN3Graph_x6 x6 noError
  , parseTest "exoticParseTest07" exoticN3Graph_x7 x7 noError
  , parseTest "exoticParseTest07a" exoticN3Graph_x7a x7 noError
    -- exoticParseTest08 = parseTest "exoticParseTest08" exoticN3Graph_x8 x8 noError
    -- exoticParseTest09 = parseTest "exoticParseTest09" exoticN3Graph_x9 x9 noError
  , parseTest "exoticParseTest12" exoticN3Graph_x12 x12 noError
    -- exoticParseTest13 = parseTest "exoticParseTest13" exoticN3Graph_x13 x13 noError
    -- exoticParseTest14 = parseTest "exoticParseTest14" exoticN3Graph_x14 x14 noError -- TODO: re-instate?
    -- exoticParseTest15 = parseTest "exoticParseTest15" exoticN3Graph_x15 x15 noError
    -- exoticParseTest16 = parseTest "exoticParseTest16" exoticN3Graph_x16 x16 noError
    -- exoticParseTest17 = parseTest "exoticParseTest17" exoticN3Graph_x17 x17 noError

  , exoticTest "01" x1
  , exoticTest "02" x2
  , exoticTest "03" x3
  , exoticTest "04" x4
  , exoticTest "05" x5
  , exoticTest "06" x6
  , exoticTest "07" x7
    -- exoticTest08 = exoticTest "08" x8 -- TODO: serialisation uses :- with a named node
    -- exoticTest09 = exoticTest "09" x9 -- TODO: serialisation uses :- with a named node
  , testGraphEq  "exoticTest10" False x7 x8
  , testGraphEq  "exoticTest11" False x8 x9
  , exoticTest "12" x12
  , exoticTest "13" x13
  , exoticTest "13a" x13a
  , exoticTest "14" x14
  , exoticTest "15" x15
  , exoticTest "16" x16
  , exoticTest "17" x17

  , fullRoundTripTest "Exotic01" exoticN3Graph_x1
  , fullRoundTripTest "Exotic02" exoticN3Graph_x2
    -- exoticRoundTripTest03 = fullRoundTripTest "Exotic03" exoticN3Graph_x3
  , fullRoundTripTest "Exotic04" exoticN3Graph_x4
  , fullRoundTripTest "Exotic05" exoticN3Graph_x5
    -- exoticRoundTripTest06 = fullRoundTripTest "Exotic06" exoticN3Graph_x6
  , fullRoundTripTest "Exotic07" exoticN3Graph_x7
    -- exoticRoundTripTest08 = fullRoundTripTest "Exotic08" exoticN3Graph_x8
    -- exoticRoundTripTest09 = fullRoundTripTest "Exotic09" exoticN3Graph_x9
  , fullRoundTripTest "Exotic12" exoticN3Graph_x12
  , fullRoundTripTest "Exotic14" exoticN3Graph_x14
    -- exoticRoundTripTest15 = fullRoundTripTest "Exotic15" exoticN3Graph_x15
    -- exoticRoundTripTest16 = fullRoundTripTest "Exotic16" exoticN3Graph_x16
    -- exoticRoundTripTest17 = fullRoundTripTest "Exotic17" exoticN3Graph_x17
    -- exoticRoundTripTest18 = fullRoundTripTest "Exotic18" exoticN3Graph_x18
    
  ]

------------------------------------------------------------
--  All tests
------------------------------------------------------------

allTests :: Test
allTests = TestList
  [ trivialTestSuite
  , parseTestSuite
  , roundTripTestSuite
  , simpleTestSuite
  , exoticTestSuite
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