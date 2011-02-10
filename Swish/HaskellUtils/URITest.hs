--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  URITest
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98 + multi-parameter classes
--
-- This Module contains test cases for module URI.
--
--------------------------------------------------------------------------------

{-

Running this with the development version of 0.3.0.0 gives the following errors:

### Failure in: 0:Test URIrefs:17:0          
testValidURIRef:http://example.123./aaa/bbb#ccc
expected: False
 but got: True
### Failure in: 0:Test URIrefs:17:1          
testAbsoluteURIRef:http://example.123./aaa/bbb#ccc
expected: False
 but got: True
### Failure in: 1:Test RFC2396 examples:22    
testRFC23
expected: "#s"
 but got: "http://a/b/c/d;p?q#s"
### Failure in: 1:Test RFC2396 examples:23    
testRFC24
expected: ""
 but got: "http://a/b/c/d;p?q"
### Failure in: 1:Test RFC2396 examples:25    
testRFC32
expected: "http://a/../g"
 but got: "http://a/g"
### Failure in: 1:Test RFC2396 examples:26    
testRFC33
expected: "http://a/../../g"
 but got: "http://a/g"
### Failure in: 1:Test RFC2396 examples:28    
testRFC35
expected: "http://a/../g"
 but got: "http://a/g"
### Failure in: 2:Test oddball examples:0     
testMail01
expected: "mailto:local@domain"
 but got: "mailto:local/local@domain"
### Failure in: 2:Test oddball examples:1     
testMail02
expected: "mailto:#newfrag"
 but got: "mailto:local/option@domain.org?notaquery#newfrag"
### Failure in: 2:Test oddball examples:2     
testMail03
expected: "mailto:l1/q1@domain"
 but got: "mailto:local/l1/q1@domain"
### Failure in: 2:Test oddball examples:8      
testMail16
expected: "mailto:?query2"
 but got: "mailto:local@domain?query2"
### Failure in: 2:Test oddball examples:9      
testInfo17
expected: "info:name/9876/../543"
 but got: "info:name/name/543"
Cases: 251  Tried: 251  Errors: 0  Failures: 12

-}

-- WNH RIP OUT module Swish.HaskellUtils.URITest where

import Test.HUnit
import Swish.HaskellUtils.Parse
import Swish.HaskellUtils.ProcessURI
import System.IO ( Handle, openFile, IOMode(WriteMode), hClose, hPutStr, hPutStrLn )

data URIType = AbsId    -- URI form (absolute, no fragment)
             | AbsRf    -- Absolute URI reference
             | RelRf    -- Relative URI reference
             | InvRf    -- Invalid URI reference
isValidT :: URIType -> Bool
isValidT InvRf = False
isValidT _     = True

isAbsRfT :: URIType -> Bool
isAbsRfT AbsId = True
isAbsRfT AbsRf = True
isAbsRfT _     = False

isAbsIdT :: URIType -> Bool
isAbsIdT AbsId = True
isAbsIdT _     = False

testURIRef :: URIType -> String -> Test
testURIRef t u = TestList
  [
    TestCase ( assertEqual ("testValidURIRef:"++u)    (isValidT t) (isValidURIRef    u) ),
    TestCase ( assertEqual ("testAbsoluteURIRef:"++u) (isAbsRfT t) (isAbsoluteURIRef u) )
  ]

testURIRef001 = testURIRef AbsRf "http://example.org/aaa/bbb#ccc"
testURIRef002 = testURIRef AbsId "mailto:local@domain.org"
testURIRef003 = testURIRef AbsRf "mailto:local@domain.org#frag"
testURIRef004 = testURIRef AbsRf "HTTP://EXAMPLE.ORG/AAA/BBB#CCC"
testURIRef005 = testURIRef RelRf "//example.org/aaa/bbb#ccc"
testURIRef006 = testURIRef RelRf "/aaa/bbb#ccc"
testURIRef007 = testURIRef RelRf "bbb#ccc"
testURIRef008 = testURIRef RelRf "#ccc"
testURIRef009 = testURIRef RelRf "#"
testURIRef010 = testURIRef RelRf "/"
-- escapes
testURIRef011 = testURIRef AbsRf "http://example.org/aaa%2fbbb#ccc"
testURIRef012 = testURIRef AbsRf "http://example.org/aaa%2Fbbb#ccc"
testURIRef013 = testURIRef RelRf "%2F"
testURIRef014 = testURIRef RelRf "aaa%2Fbbb"
-- ports
testURIRef015 = testURIRef AbsRf "http://example.org:80/aaa/bbb#ccc"
testURIRef016 = testURIRef AbsRf "http://example.org:/aaa/bbb#ccc"
testURIRef017 = testURIRef AbsRf "http://example.org./aaa/bbb#ccc"
testURIRef018 = testURIRef InvRf "http://example.123./aaa/bbb#ccc"
-- IPv6 literals (from RFC2732):
testURIRef021 = testURIRef AbsId "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html"
testURIRef022 = testURIRef AbsId "http://[1080:0:0:0:8:800:200C:417A]/index.html"
testURIRef023 = testURIRef AbsId "http://[3ffe:2a00:100:7031::1]"
testURIRef024 = testURIRef AbsId "http://[1080::8:800:200C:417A]/foo"
testURIRef025 = testURIRef AbsId "http://[::192.9.5.5]/ipng"
testURIRef026 = testURIRef AbsId "http://[::FFFF:129.144.52.38]:80/index.html"
testURIRef027 = testURIRef AbsId "http://[2010:836B:4179::836B:4179]"
testURIRef028 = testURIRef RelRf "//[2010:836B:4179::836B:4179]"
testURIRef029 = testURIRef InvRf "[2010:836B:4179::836B:4179]"
-- RFC2396 test cases
testURIRef031 = testURIRef RelRf "./aaa"
testURIRef032 = testURIRef RelRf "../aaa"
testURIRef033 = testURIRef AbsId "g:h"
testURIRef034 = testURIRef RelRf "g"
testURIRef035 = testURIRef RelRf "./g"
testURIRef036 = testURIRef RelRf "g/"
testURIRef037 = testURIRef RelRf "/g"
testURIRef038 = testURIRef RelRf "//g"
testURIRef039 = testURIRef RelRf "?y"
testURIRef040 = testURIRef RelRf "g?y"
testURIRef041 = testURIRef RelRf "#s"
testURIRef042 = testURIRef RelRf "g#s"
testURIRef043 = testURIRef RelRf "g?y#s"
testURIRef044 = testURIRef RelRf ";x"
testURIRef045 = testURIRef RelRf "g;x"
testURIRef046 = testURIRef RelRf "g;x?y#s"
testURIRef047 = testURIRef RelRf "."
testURIRef048 = testURIRef RelRf "./"
testURIRef049 = testURIRef RelRf ".."
testURIRef050 = testURIRef RelRf "../"
testURIRef051 = testURIRef RelRf "../g"
testURIRef052 = testURIRef RelRf "../.."
testURIRef053 = testURIRef RelRf "../../"
testURIRef054 = testURIRef RelRf "../../g"
testURIRef055 = testURIRef RelRf "../../../g"
testURIRef056 = testURIRef RelRf "../../../../g"
testURIRef057 = testURIRef RelRf "/./g"
testURIRef058 = testURIRef RelRf "/../g"
testURIRef059 = testURIRef RelRf "g."
testURIRef060 = testURIRef RelRf ".g"
testURIRef061 = testURIRef RelRf "g.."
testURIRef062 = testURIRef RelRf "..g"
testURIRef063 = testURIRef RelRf "./../g"
testURIRef064 = testURIRef RelRf "./g/."
testURIRef065 = testURIRef RelRf "g/./h"
testURIRef066 = testURIRef RelRf "g/../h"
testURIRef067 = testURIRef RelRf "g;x=1/./y"
testURIRef068 = testURIRef RelRf "g;x=1/../y"
testURIRef069 = testURIRef RelRf "g?y/./x"
testURIRef070 = testURIRef RelRf "g?y/../x"
testURIRef071 = testURIRef RelRf "g#s/./x"
testURIRef072 = testURIRef RelRf "g#s/../x"
-- Invalid
testURIRef081 = testURIRef RelRf ""
testURIRef082 = testURIRef InvRf " "
testURIRef083 = testURIRef InvRf "%"
testURIRef084 = testURIRef InvRf "A%Z"
testURIRef085 = testURIRef InvRf "%ZZ"
testURIRef086 = testURIRef InvRf "%AZ"
testURIRef087 = testURIRef InvRf "A C"
testURIRef088 = testURIRef InvRf "A\"C"
testURIRef089 = testURIRef RelRf "A'C"
testURIRef090 = testURIRef InvRf "A\"C"
testURIRef091 = testURIRef InvRf "A`C"
testURIRef092 = testURIRef InvRf "A<C"
testURIRef093 = testURIRef InvRf "A>C"
testURIRef094 = testURIRef InvRf "A^C"
testURIRef095 = testURIRef InvRf "A\\C"
testURIRef096 = testURIRef InvRf "A{C"
testURIRef097 = testURIRef InvRf "A|C"
testURIRef098 = testURIRef InvRf "A}C"
-- From RFC2396:
-- rel_segment   = 1*( unreserved | escaped |
--                     ";" | "@" | "&" | "=" | "+" | "$" | "," )
-- unreserved    = alphanum | mark
-- mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
--                 "(" | ")"
-- Note RFC 2732 allows '[', ']' ONLY for reserved purpose of IPv6 literals,
-- or does it?
testURIRef101 = testURIRef InvRf "A[C"
testURIRef102 = testURIRef InvRf "A]C"
testURIRef103 = testURIRef InvRf "A[**]C"
testURIRef104 = testURIRef InvRf "http://[xyz]/"
testURIRef105 = testURIRef InvRf "http://]/"
testURIRef106 = testURIRef InvRf "http://example.org/[2010:836B:4179::836B:4179]"
testURIRef107 = testURIRef InvRf "http://example.org/abc#[2010:836B:4179::836B:4179]"
testURIRef108 = testURIRef InvRf "http://example.org/xxx/[qwerty]#a[b]"
-- Random other things that crop up
testURIRef111 = testURIRef AbsRf "http://example/Andr&#567;"

testURIRefSuite = TestLabel "Test URIrefs" testURIRefList
testURIRefList = TestList
  [
    testURIRef001, testURIRef002, testURIRef003, testURIRef004,
    testURIRef005, testURIRef006, testURIRef007, testURIRef008,
    testURIRef009, testURIRef010,
    --
    testURIRef011, testURIRef012, testURIRef013, testURIRef014,
    testURIRef015, testURIRef016, testURIRef017, testURIRef018,
    --
    testURIRef021, testURIRef022, testURIRef023, testURIRef024,
    testURIRef025, testURIRef026, testURIRef027, testURIRef028,
    testURIRef029,
    --
    testURIRef031, testURIRef032, testURIRef033, testURIRef034,
    testURIRef035, testURIRef036, testURIRef037, testURIRef038,
    testURIRef039,
    testURIRef040, testURIRef041, testURIRef042, testURIRef043,
    testURIRef044, testURIRef045, testURIRef046, testURIRef047,
    testURIRef048, testURIRef049,
    testURIRef050, testURIRef051, testURIRef052, testURIRef053,
    testURIRef054, testURIRef055, testURIRef056, testURIRef057,
    testURIRef058, testURIRef059,
    testURIRef060, testURIRef061, testURIRef062, testURIRef063,
    testURIRef064, testURIRef065, testURIRef066, testURIRef067,
    testURIRef068, testURIRef069,
    testURIRef070, testURIRef071, testURIRef072,
    --
    testURIRef081, testURIRef082, testURIRef083, testURIRef084,
    testURIRef085, testURIRef086, testURIRef087, testURIRef088,
    testURIRef089,
    testURIRef090, testURIRef091, testURIRef092, testURIRef093,
    testURIRef094, testURIRef095, testURIRef096, testURIRef097,
    testURIRef098, -- testURIRef099,
    --
    testURIRef101, testURIRef102, testURIRef103, testURIRef104,
    testURIRef105, testURIRef106, testURIRef107, testURIRef108,
    --
    testURIRef111
  ]

-- Get reference relative to given base
--   relativeRef :: String -> String -> String
-- Get absolute URI given base and relative reference
--   absoluteURI :: String -> String -> String
--
-- Test cases taken from: http://www.w3.org/2000/10/swap/uripath.py
-- (Thanks, Dan Connolly)
--
-- NOTE:  absoluteURI base (relativeRef base u) is always equivalent to u.
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html

testRelJoin  :: String -> String -> String -> String -> Test
testRelJoin label base urel uabs =
    TestCase ( assertEqual label uabs ( absoluteUriPart base urel ) )

-- RFC2396 relative-to-absolute URI tests

rfcbase  = "http://a/b/c/d;p?q"
-- normal cases, RFC2396 C.1
testRFC01 = testRelJoin "testRFC01" rfcbase "g:h" "g:h"
testRFC02 = testRelJoin "testRFC02" rfcbase "g" "http://a/b/c/g"
testRFC03 = testRelJoin "testRFC03" rfcbase "./g" "http://a/b/c/g"
testRFC04 = testRelJoin "testRFC04" rfcbase "g/" "http://a/b/c/g/"
testRFC05 = testRelJoin "testRFC05" rfcbase "/g" "http://a/g"
testRFC06 = testRelJoin "testRFC06" rfcbase "//g" "http://g"
testRFC07 = testRelJoin "testRFC07" rfcbase "?y" "http://a/b/c/d;p?y"
testRFC08 = testRelJoin "testRFC08" rfcbase "g?y" "http://a/b/c/g?y"
testRFC09 = testRelJoin "testRFC09" rfcbase "?q#s" "http://a/b/c/d;p?q#s"
testRFC10 = testRelJoin "testRFC10" rfcbase "g#s" "http://a/b/c/g#s"
testRFC11 = testRelJoin "testRFC11" rfcbase "g?y#s" "http://a/b/c/g?y#s"
testRFC12 = testRelJoin "testRFC12" rfcbase ";x" "http://a/b/c/;x"
testRFC13 = testRelJoin "testRFC13" rfcbase "g;x" "http://a/b/c/g;x"
testRFC14 = testRelJoin "testRFC14" rfcbase "g;x?y#s" "http://a/b/c/g;x?y#s"
testRFC15 = testRelJoin "testRFC15" rfcbase "." "http://a/b/c/"
testRFC16 = testRelJoin "testRFC16" rfcbase "./" "http://a/b/c/"
testRFC17 = testRelJoin "testRFC17" rfcbase ".." "http://a/b/"
testRFC18 = testRelJoin "testRFC18" rfcbase "../" "http://a/b/"
testRFC19 = testRelJoin "testRFC19" rfcbase "../g" "http://a/b/g"
testRFC20 = testRelJoin "testRFC20" rfcbase "../.." "http://a/"
testRFC21 = testRelJoin "testRFC21" rfcbase "../../" "http://a/"
testRFC22 = testRelJoin "testRFC22" rfcbase "../../g" "http://a/g"
testRFC23 = testRelJoin "testRFC23" rfcbase "#s" "#s"   -- current document
testRFC24 = testRelJoin "testRFC24" rfcbase "" ""       -- current document
-- abnormal cases, RFC2396 C.2
testRFC31 = testRelJoin "testRFC31" rfcbase "?q" rfcbase
testRFC32 = testRelJoin "testRFC32" rfcbase "../../../g" "http://a/../g"
testRFC33 = testRelJoin "testRFC33" rfcbase "../../../../g" "http://a/../../g"
testRFC34 = testRelJoin "testRFC34" rfcbase "/./g" "http://a/g"
--testRFC34 = testRelJoin "testRFC34" rfcbase "/./g" "http://a/./g"  -- RFC2396 says don't remove '.'
testRFC35 = testRelJoin "testRFC35" rfcbase "/../g" "http://a/../g"
testRFC36 = testRelJoin "testRFC36" rfcbase "g." "http://a/b/c/g."
testRFC37 = testRelJoin "testRFC37" rfcbase ".g" "http://a/b/c/.g"
testRFC38 = testRelJoin "testRFC38" rfcbase "g.." "http://a/b/c/g.."
testRFC39 = testRelJoin "testRFC39" rfcbase "..g" "http://a/b/c/..g"
testRFC40 = testRelJoin "testRFC40" rfcbase "./../g" "http://a/b/g"
testRFC41 = testRelJoin "testRFC41" rfcbase "./g/." "http://a/b/c/g/"
testRFC42 = testRelJoin "testRFC42" rfcbase "g/./h" "http://a/b/c/g/h"
testRFC43 = testRelJoin "testRFC43" rfcbase "g/../h" "http://a/b/c/h"
testRFC44 = testRelJoin "testRFC44" rfcbase "g;x=1/./y" "http://a/b/c/g;x=1/y"
testRFC45 = testRelJoin "testRFC45" rfcbase "g;x=1/../y" "http://a/b/c/y"
testRFC46 = testRelJoin "testRFC46" rfcbase "g?y/./x" "http://a/b/c/g?y/./x"
testRFC47 = testRelJoin "testRFC47" rfcbase "g?y/../x" "http://a/b/c/g?y/../x"
testRFC48 = testRelJoin "testRFC48" rfcbase "g#s/./x" "http://a/b/c/g#s/./x"
testRFC49 = testRelJoin "testRFC49" rfcbase "g#s/../x" "http://a/b/c/g#s/../x"
testRFC50 = testRelJoin "testRFC50" rfcbase "http:x" "http:x"

-- Null path tests
-- See RFC2396bis, section 5.2,
-- "If the base URI's path component is the empty string, then a single
--  slash character is copied to the buffer"
-- testRFC60 = testRelative "testRFC60" "http://ex"     "http://ex/x/y?q" "/x/y?q"
testRFC61 = testRelJoin  "testRFC61" "http://ex"     "x/y?q"           "http://ex/x/y?q"
-- testRFC62 = testRelative "testRFC62" "http://ex?p"   "http://ex/x/y?q" "/x/y?q"
testRFC63 = testRelJoin  "testRFC63" "http://ex?p"   "x/y?q"           "http://ex/x/y?q"
-- testRFC64 = testRelative "testRFC64" "http://ex#f"   "http://ex/x/y?q" "/x/y?q"
testRFC65 = testRelJoin  "testRFC65" "http://ex#f"   "x/y?q"           "http://ex/x/y?q"
-- testRFC66 = testRelative "testRFC66" "http://ex?p"   "http://ex/x/y#g" "/x/y#g"
testRFC67 = testRelJoin  "testRFC67" "http://ex?p"   "x/y#g"           "http://ex/x/y#g"
-- testRFC68 = testRelative "testRFC68" "http://ex"     "http://ex/"      "/"
testRFC69 = testRelJoin  "testRFC69" "http://ex"     "./"              "http://ex/"
-- testRFC70 = testRelative "testRFC70" "http://ex"     "http://ex/a/b"   "/a/b"
-- testRFC71 = testRelative "testRFC71" "http://ex/a/b" "http://ex"       "./"

testRFC2396Suite = TestLabel "Test RFC2396 examples" testRFC2396List
testRFC2396List  = TestList
  [
    testRFC01, testRFC02, testRFC03, testRFC04,
    testRFC05, testRFC06, testRFC07, testRFC08,
    testRFC09,
    testRFC10, testRFC11, testRFC12, testRFC13,
    testRFC14, testRFC15, testRFC16, testRFC17,
    testRFC18, testRFC19,
    testRFC20, testRFC21, testRFC22, testRFC23,
    testRFC24,
    -- testRFC30,
    testRFC31, testRFC32, testRFC33,
    testRFC34, testRFC35, testRFC36, testRFC37,
    testRFC38, testRFC39,
    testRFC40, testRFC41, testRFC42, testRFC43,
    testRFC44, testRFC45, testRFC46, testRFC47,
    testRFC48, testRFC49,
    testRFC50,
    --
    testRFC61, testRFC63, testRFC65, testRFC67,
    testRFC69
  ]

-- And some other oddballs:
mailbase = "mailto:local/option@domain.org?notaquery#frag"
testMail01 = testRelJoin "testMail01"
            mailbase "local@domain"
            "mailto:local@domain"
testMail02 = testRelJoin "testMail02"
            mailbase "#newfrag"
            "mailto:#newfrag"
            -- "mailto:local/option@domain.org?notaquery#newfrag"
testMail03 = testRelJoin "testMail03"
            mailbase "l1/q1@domain"
            "mailto:l1/q1@domain"

testMail11 = testRelJoin "testMail11"
             "mailto:local1@domain1?query1" "mailto:local2@domain2"
             "mailto:local2@domain2"
testMail12 = testRelJoin "testMail12"
             "mailto:local1@domain1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail13 = testRelJoin "testMail13"
             "mailto:local1@domain1?query1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail14 = testRelJoin "testMail14"
             "mailto:local@domain?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail15 = testRelJoin "testMail15"
             "mailto:?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail16 = testRelJoin "testMail16"
             "mailto:local@domain?query1" "?query2"
             "mailto:?query2"
testInfo17 = testRelJoin "testInfo17"
             "info:name/1234/../567" "name/9876/../543"
             "info:name/9876/../543"
testInfo18 = testRelJoin "testInfo18"
             "info:/name/1234/../567" "name/9876/../543"
             "info:/name/name/543"

testOddballSuite = TestLabel "Test oddball examples" testOddballList
testOddballList  = TestList
  [ testMail01, testMail02, testMail03
  , testMail11, testMail12, testMail13, testMail14, testMail15, testMail16
  , testInfo17
  ]

-- Full test suite
allTests = TestList
  [ testURIRefSuite,
    testRFC2396Suite,
    testOddballSuite
  ]

main = runTestTT allTests

runTestFile t = do
    h <- openFile "a.tmp" WriteMode
    runTestText (putTextToHandle h False) t
    hClose h
tf = runTestFile
tt = runTestTT

uref = testURIRefSuite
rfc  = testRFC2396Suite
oddb = testOddballSuite

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
