{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  TurtleParserTest
--  Copyright   :  (c) 2013 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This Module contains test cases for module "TurtleParser", based on the
--  examples given in <http://www.w3.org/TR/2013/CR-turtle-20130219/>.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Maybe (fromMaybe)

import Network.URI (URI, parseURIReference)

import Swish.RDF.Graph ( RDFGraph
                        , RDFLabel (..)
                        , RDFTriple
                        , ToRDFLabel
                        -- , getArcs
                        , toRDFGraph
                        , toRDFTriple
                        )
                          
import Swish.RDF.Parser.Turtle (parseTurtle)
import Swish.RDF.Vocabulary.DublinCore (dcelemtitle)
import Swish.RDF.Vocabulary.FOAF (foafgivenName, foafknows, foafmbox, foafname, foafPerson)
import Swish.RDF.Vocabulary.RDF (rdfType, rdfFirst, rdfRest, rdfNil, rdfsLabel)
import Swish.RDF.Vocabulary.XSD (xsdDecimal, xsdString)

import Test.HUnit (Test(TestCase,TestList), (~=?), (~:), assertFailure)

import TestHelpers (runTestSuite)

triple :: (ToRDFLabel s, ToRDFLabel p, ToRDFLabel o)
          => s -> p -> o -> RDFTriple
triple = toRDFTriple

toURI :: String -> URI
toURI s = fromMaybe (error ("Internal error: invalid uri=" ++ s)) (parseURIReference s)

toGraph :: T.Text -> Either String RDFGraph
toGraph = flip parseTurtle Nothing . L.fromStrict

-- It is quicker to just check the statements when we
-- know that the graph can not contain a blank node
-- - i.e. compare getArcs gr to S.fromList o - but
-- stick with graph equality checks, for now.
compareExample :: String -> T.Text -> [RDFTriple] -> Test
compareExample l t o =
  let egr = toGraph t
      oset = S.fromList o
      lbl = "example: " ++ l
  in case egr of
    Left e -> TestCase $ assertFailure $ "Unable to parse example " ++ l ++ ":\n" ++ e
    -- Right gr -> lbl ~: oset ~=? getArcs gr
    Right gr -> lbl ~: toRDFGraph oset ~=? gr

compareGraphs :: String -> T.Text -> T.Text -> Test
compareGraphs l t1 t2 =
  case (toGraph t1, toGraph t2) of
    (Left e1, _) -> TestCase $ assertFailure $ "Unable to parse graph 1 of " ++ l ++ ":\n" ++ e1
    (_, Left e2) -> TestCase $ assertFailure $ "Unable to parse graph 2 of " ++ l ++ ":\n" ++ e2
    -- (Right gr1, Right gr2) -> ("example: " ++ l) ~: (getArcs gr1) ~=? (getArcs gr2)
    (Right gr1, Right gr2) -> ("example: " ++ l) ~: gr1 ~=? gr2
      
-- *********************************************
-- Examples from Turtle specification
-- *********************************************

--   would be nice to read these in from external files

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-intro>.
example1 :: T.Text
example1 =
  T.unlines
  [ "@base <http://example.org/> ."
  , "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  , "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
  , "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  , "@prefix rel: <http://www.perceive.net/schemas/relationship/> ."
  , ""
  , "<#green-goblin>"
  , "    rel:enemyOf <#spiderman> ;"
  , "    a foaf:Person ;    # in the context of the Marvel universe"
  , "    foaf:name \"Green Goblin\" ."
  , ""
  , "<#spiderman>"
  , "    rel:enemyOf <#green-goblin> ;"
  , "    a foaf:Person ;"
  , "    foaf:name \"Spiderman\", \"Человек-паук\"@ru ."
  ]

result1 :: [RDFTriple]
result1 =
  let greengoblin = toURI "http://example.org/#green-goblin"
      spiderman = toURI "http://example.org/#spiderman"
      enemyOf = toURI "http://www.perceive.net/schemas/relationship/enemyOf"
      ruName = LangLit "Человек-паук" "ru"
  in [ triple greengoblin enemyOf spiderman
     , triple greengoblin rdfType foafPerson
     , triple greengoblin foafname ("Green Goblin"::String)
     , triple spiderman enemyOf greengoblin
     , triple spiderman rdfType foafPerson
     , triple spiderman foafname ("Spiderman"::String)
     , triple spiderman foafname ruName
     ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-iri>.
-- 
--   Unfortunately we do not support IRIs at this time.
example2_4 :: T.Text
example2_4 = 
  T.unlines
  [ "# A triple with all absolute IRIs"
  , "<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> ."
  , ""
  , "@base <http://one.example/> ."
  , "<subject2> <predicate2> <object2> .     # relative IRIs, e.g. http://one.example/subject2"
  , ""
  , "@prefix p: <http://two.example/> ."
  , "p:subject3 p:predicate3 p:object3 .     # prefixed name, e.g. http://two.example/subject3"
  , ""
  , "@prefix p: <path/> .                    # prefix p: now stands for http://one.example/path/"
  , "p:subject4 p:predicate4 p:object4 .     # prefixed name, e.g. http://one.example/path/subject4"
  , ""
  , "@prefix : <http://another.example/> .    # empty prefix"
  , ":subject5 :predicate5 :object5 .        # prefixed name, e.g. http://another.example/subject5"
  , ""
  , ":subject6 a :subject7 .                 # same as :subject6 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> :subject7 ."
  , ""
  , "# <http://伝言.example/?user=أكرم&amp;channel=R%26D> a :subject8 . # a multi-script subject IRI ."
  ]

result2_4 :: [RDFTriple]
result2_4 =
  let s1 = toURI "http://one.example/subject1"
      p1 = toURI "http://one.example/predicate1"
      o1 = toURI "http://one.example/object1"

      s2 = toURI "http://one.example/subject2"
      p2 = toURI "http://one.example/predicate2"
      o2 = toURI "http://one.example/object2"

      s3 = toURI "http://two.example/subject3"
      p3 = toURI "http://two.example/predicate3"
      o3 = toURI "http://two.example/object3"
      
      s4 = toURI "http://one.example/path/subject4"
      p4 = toURI "http://one.example/path/predicate4"
      o4 = toURI "http://one.example/path/object4"
      
      s5 = toURI "http://another.example/subject5"
      p5 = toURI "http://another.example/predicate5"
      o5 = toURI "http://another.example/object5"
      
      s6 = toURI "http://another.example/subject6"
      s7 = toURI "http://another.example/subject7"

      -- Replace once we support IRIs
      -- utf8 = toURI "http://伝言.example/?user=أكرم&amp;channel=R%26D"
      -- s8 = toURI "http://another.example/subject8"
      
  in [ triple s1 p1 o1
     , triple s2 p2 o2
     , triple s3 p3 o3
     , triple s4 p4 o4
     , triple s5 p5 o5
     , triple s6 rdfType s7
     -- , triple utf8 rdfType s8
     ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#turtle-literals>.
-- 
--   *NOTE*: at present we do not match the document since untyped strings are
--   *not* mapped to xsd:string.
--
example2_5_1 :: T.Text
example2_5_1 =
  T.unlines
  [ "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ."
  , "@prefix show: <http://example.org/vocab/show/> ."
  , "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ." -- added to example
  , ""
  , "show:218 rdfs:label \"That Seventies Show\"^^xsd:string .            # literal with XML Schema string datatype"
  , "show:218 rdfs:label \"That Seventies Show\"^^<http://www.w3.org/2001/XMLSchema#string> . # same as above"
  , "show:218 rdfs:label \"That Seventies Show\" .                                            # same again"
  , "show:218 show:localName \"That Seventies Show\"@en .                 # literal with a language tag"
  , "show:218 show:localName 'Cette Série des Années Soixante-dix'@fr . # literal delimited by single quote"
  , "show:218 show:localName \"Cette Série des Années Septante\"@fr-be .  # literal with a region subtag"
  , "show:218 show:blurb '''This is a multi-line                        # literal with embedded new lines and quotes"
  , "literal with many quotes (\"\"\"\"\")"
  , "and up to two sequential apostrophes ('').''' ."
  ]

result2_5_1 :: [RDFTriple]
result2_5_1 =
  let show218 = toURI "http://example.org/vocab/show/218"
      localName = toURI "http://example.org/vocab/show/localName"
      blurb = toURI "http://example.org/vocab/show/blurb"
      
      -- Should be TypedLit v xsdString; once this happens then nameLit
      -- is not needed
      name = "That Seventies Show"
      nameLit = Lit name
      blurbLit = Lit "This is a multi-line                        # literal with embedded new lines and quotes\nliteral with many quotes (\"\"\"\"\")\nand up to two sequential apostrophes ('')."
      
  in [ triple show218 rdfsLabel (TypedLit name xsdString)
     , triple show218 rdfsLabel nameLit
     , triple show218 localName (LangLit name "en")
     , triple show218 localName (LangLit "Cette Série des Années Soixante-dix" "fr")
     , triple show218 localName (LangLit "Cette Série des Années Septante" "fr-be")
     , triple show218 blurb blurbLit
     ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#abbrev>.
example2_5_2 :: T.Text
example2_5_2 =
  T.unlines
  [ "@prefix : <http://example.org/elements> ."
  , "<http://en.wikipedia.org/wiki/Helium>              "
  , "    :atomicNumber 2 ;               # xsd:integer  "
  , "    :atomicMass 4.002602 ;          # xsd:decimal  "
  , "    :specificGravity 1.663E-4 .     # xsd:double"
  ]
  
result2_5_2 :: [RDFTriple]
result2_5_2 =
  let helium = toURI "http://en.wikipedia.org/wiki/Helium"
      aNum = toURI "http://example.org/elementsatomicNumber"
      aMass = toURI "http://example.org/elementsatomicMass"
      sGrav = toURI "http://example.org/elementsspecificGravity"

  in [ triple helium aNum (2::Int)
     , triple helium aMass (TypedLit "4.002602" xsdDecimal)
     , triple helium sGrav (1.663e-4::Double)
     ]
     
-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#booleans>.
example2_5_3 :: T.Text
example2_5_3 =
  T.unlines
  [ "@prefix : <http://example.org/stats> ."
  , "<http://somecountry.example/census2007>"
  , "    :isLandlocked false .           # xsd:boolean"
  ]
  
result2_5_3 :: [RDFTriple]
result2_5_3 =
  [ triple
    (toURI "http://somecountry.example/census2007")
    (toURI "http://example.org/statsisLandlocked")
    False
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#BNodes>.
example2_6 :: T.Text
example2_6 =
  T.unlines
  [ "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  , ""
  , "_:alice foaf:knows _:bob ."
  , "_:bob foaf:knows _:alice .    "
  ]

result2_6 :: [RDFTriple]
result2_6 =
  let alice = Blank "one"
      bob = Blank "two"

  in [ triple alice foafknows bob
     , triple bob foafknows alice 
     ]
     
-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#unlabeled-bnodes>.
example2_7_a :: T.Text
example2_7_a =
  T.unlines
  [ "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  , ""
  , "# Someone knows someone else, who has the name \"Bob\"."
  , "[] foaf:knows [ foaf:name \"Bob\" ] ."
  ]
  
result2_7_a :: [RDFTriple]
result2_7_a =
  let someone = Blank "p"
      bob = Blank "23"

  in [ triple someone foafknows bob
     , triple bob foafname (Lit "Bob") -- TODO: change to TypedLit
     ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#unlabeled-bnodes>.
example2_7_b1 :: T.Text
example2_7_b1 =
  T.unlines
  [ "@prefix foaf: <http://xmlns.com/foaf/0.1/> ."
  , ""
  , "[ foaf:name \"Alice\" ] foaf:knows ["
  , "    foaf:name \"Bob\" ;"
  , "    foaf:knows ["
  , "        foaf:name \"Eve\" ] ;"
  , "    foaf:mbox <bob@example.com> ] ."
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#unlabeled-bnodes>.
example2_7_b2 :: T.Text
example2_7_b2 =
  T.unlines
  [ "_:a <http://xmlns.com/foaf/0.1/name> \"Alice\" ."
  , "_:a <http://xmlns.com/foaf/0.1/knows> _:b ."
  , "_:b <http://xmlns.com/foaf/0.1/name> \"Bob\" ."
  , "_:b <http://xmlns.com/foaf/0.1/knows> _:c ."
  , "_:c <http://xmlns.com/foaf/0.1/name> \"Eve\" ."
  , "_:b <http://xmlns.com/foaf/0.1/mbox> <bob@example.com> ."
  ]
  
-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#collections>.
example2_8 :: T.Text
example2_8 =
  T.unlines
  [ "@prefix : <http://example.org/foo> ."
  , "# the object of this triple is the RDF collection blank node"
  , ":subject :predicate ( :a :b :c ) ."
  , ""
  , "# an empty collection value - rdf:nil"
  , ":subject :predicate2 () ."
  ]
  
result2_8 :: [RDFTriple]
result2_8 =
  let s = toURI "http://example.org/foosubject"
      p = toURI "http://example.org/foopredicate"
      p2 = toURI "http://example.org/foopredicate2"

      a = toURI "http://example.org/fooa"
      b = toURI "http://example.org/foob"
      c = toURI "http://example.org/fooc"

      l1 = Blank "l1"
      l2 = Blank "l2"
      l3 = Blank "l3"
      
  in [ triple s p l1
     , triple l1 rdfFirst a
     , triple l1 rdfRest l2
     , triple l2 rdfFirst b
     , triple l2 rdfRest l3
     , triple l3 rdfFirst c
     , triple l3 rdfRest rdfNil
     , triple s p2 rdfNil
     ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-examples>.
example3_a :: T.Text
example3_a =
  T.unlines
  [ "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  , "@prefix dc: <http://purl.org/dc/elements/1.1/> ."
  , "@prefix ex: <http://example.org/stuff/1.0/> ."
  , ""
  , "<http://www.w3.org/TR/rdf-syntax-grammar>"
  , "  dc:title \"RDF/XML Syntax Specification (Revised)\" ;"
  , "  ex:editor ["
  , "    ex:fullname \"Dave Beckett\";"
  , "    ex:homePage <http://purl.org/net/dajobe/>"
  , "  ] ."
  ]

result3_a :: [RDFTriple]
result3_a =
  let g = toURI "http://www.w3.org/TR/rdf-syntax-grammar"
      exEditor = toURI "http://example.org/stuff/1.0/editor"
      exFullName = toURI "http://example.org/stuff/1.0/fullname"
      exHomePage = toURI "http://example.org/stuff/1.0/homePage"
      bn = Blank "$123_"
      
  in [ triple g dcelemtitle (Lit "RDF/XML Syntax Specification (Revised)")
     , triple g exEditor bn
     , triple bn exFullName (Lit "Dave Beckett")
     , triple bn exHomePage (toURI "http://purl.org/net/dajobe/")
     ]
     
-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-examples>.
example3_b1 :: T.Text
example3_b1 =
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , ":a :b ( \"apple\" \"banana\" ) ."
  ]

example3_b2 :: T.Text
example3_b2 =
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  , ":a :b"
  , "  [ rdf:first \"apple\";"
  , "    rdf:rest [ rdf:first \"banana\";"
  , "               rdf:rest rdf:nil ]"
  , "  ] .    "
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-examples>.
example3_c1 :: T.Text
example3_c1 =
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , ""
  , ":a :b \"The first line\\nThe second line\\n  more\" ."
  ]

example3_c2 :: T.Text
example3_c2 = 
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , ""
    -- line feeds using U+000A
  , ":a :b \"\"\"The first line"
  , "The second line"
  , "  more\"\"\" ."
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-examples>.
example3_d1 :: T.Text
example3_d1 =
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , "(1 2.0 3E1) :p \"w\" ."
  ]

example3_d2 :: T.Text
example3_d2 =
  T.unlines
  [ "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  , "@prefix : <http://example.org/stuff/1.0/> ."
  , "_:b0  rdf:first  1 ;"
  , "      rdf:rest   _:b1 ."
  , "_:b1  rdf:first  2.0 ;"
  , "      rdf:rest   _:b2 ."
  , "_:b2  rdf:first  3E1 ;"
  , "      rdf:rest   rdf:nil ."
  , "_:b0  :p         \"w\" . "
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-examples>.
example3_e1 :: T.Text
example3_e1 =
  T.unlines
  [ "@prefix : <http://example.org/stuff/1.0/> ."
  , "(1 [:p :q] ( 2 ) ) ."
  ]
  
example3_e2 :: T.Text
example3_e2 =
  T.unlines
  [ "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  , "@prefix : <http://example.org/stuff/1.0/> ."
  , "    _:b0  rdf:first  1 ;"
  , "          rdf:rest   _:b1 ."
  , "    _:b1  rdf:first  _:b2 ."
  , "    _:b2  :p         :q ."
  , "    _:b1  rdf:rest   _:b3 ."
  , "    _:b3  rdf:first  _:b4 ."
  , "    _:b4  rdf:first  2 ;"
  , "          rdf:rest   rdf:nil ."
  , "    _:b3  rdf:rest   rdf:nil ."
  ]

-- | From <http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-parsing-example>.
example7_4 :: T.Text
example7_4 =
  T.unlines
  [ "@prefix ericFoaf: <http://www.w3.org/People/Eric/ericP-foaf.rdf#> ."
  , "@prefix : <http://xmlns.com/foaf/0.1/> ."
  , "ericFoaf:ericP :givenName \"Eric\" ;"
  , "              :knows <http://norman.walsh.name/knows/who/dan-brickley> ,"
  , "                      [ :mbox <mailto:timbl@w3.org> ] ,"
  , "                      <http://getopenid.com/amyvdh> ."
  ]

result7_4 :: [RDFTriple]
result7_4 =
  let ericP = toURI "http://www.w3.org/People/Eric/ericP-foaf.rdf#ericP"
      db = toURI "http://norman.walsh.name/knows/who/dan-brickley"
      timbl = toURI "mailto:timbl@w3.org"
      amyvdh = toURI "http://getopenid.com/amyvdh"
      timbn = Blank "node"
      
  in [ triple ericP foafgivenName (Lit "Eric")
     , triple ericP foafknows db
     , triple ericP foafknows timbn
     , triple ericP foafknows amyvdh
     , triple timbn foafmbox timbl
     ]
     
-- *********************************************
-- Set up test suites
-- *********************************************

initialTestSuite :: Test
initialTestSuite =
  TestList
  [ compareExample "1" example1 result1
  , compareExample "2.4" example2_4 result2_4
  , compareExample "2.5.1" example2_5_1 result2_5_1
  , compareExample "2.5.2" example2_5_2 result2_5_2
  , compareExample "2.5.3" example2_5_3 result2_5_3
  , compareExample "2.6" example2_6 result2_6
  , compareExample "2.7 a" example2_7_a result2_7_a
  , compareGraphs  "2.7 b" example2_7_b1 example2_7_b2
  , compareExample "2.8" example2_8 result2_8
  , compareExample "3 a" example3_a result3_a
  , compareGraphs  "3 b" example3_b1 example3_b2
  , compareGraphs  "3 c" example3_c1 example3_c2
  , compareGraphs  "3 d" example3_d1 example3_d2
  -- , compareGraphs  "3 e" example3_e1 example3_e2 -- at present _e1 does not parse
  , compareExample "7.4" example7_4 result7_4
  ]

-- Cases to try and improve the test coverage

-- | This was actually more a problem with output rather than input.
coverage1 :: T.Text
coverage1 =
  T.unlines
  [ "<urn:a> <urn:b> \"' -D RT @madeupname: \\\"Foo \\u0024 Zone\\\" \\U0000007e:\\\"\\\"\\\"D\" ."
  ]

resultc1 :: [RDFTriple]
resultc1 =
  [ triple
    (toURI "urn:a")
    (toURI "urn:b")
    (Lit "' -D RT @madeupname: \"Foo $ Zone\" ~:\"\"\"D")
  ]

coverageCases :: Test
coverageCases =
  TestList
  [ compareExample "p1" coverage1 resultc1
  ]
  
allTests :: Test
allTests = TestList
  [ initialTestSuite
  , coverageCases
  ]

main :: IO ()
main = runTestSuite allTests

--------------------------------------------------------------------------------
--
--  Copyright (c) 2013 Douglas Burke
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
