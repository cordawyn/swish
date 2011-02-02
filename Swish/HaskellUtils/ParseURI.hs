--------------------------------------------------------------------------------
--  $Id: ParseURI.hs,v 1.1 2004/01/13 12:31:24 graham Exp $
--
--  Copyright (c) 2003, G. KLYNE.  All rights reserved.
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ParseURI
--  Copyright   :  (c) 2003, Graham Klyne
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines a collection of functions for parsing URIs.
--
--  These are used mainly as helper functions by the module URI.
--
--  The current official reference for URI handling is RFC2396 [1],
--  as updated by RFC 2732 [2].
--
--  These are being merged into RFC2396bis [3], a work-in-progress copy of
--  which is available at the URI indicated.  This document has been used
--  as the primary reference for constructing the URI parser implemented
--  here, and it is intended that there is a direct relationship between
--  the syntax definition in that document and the parser implementation.
--
--  [1] http://www.ietf.org/rfc/rfc2396.txt
--  [2] http://www.ietf.org/rfc/rfc2732.txt
--  [3] http://www.apache.org/~fielding/uri/rev-2002/rfc2396bis.html
--
--------------------------------------------------------------------------------

module Swish.HaskellUtils.ParseURI
  ( URI(URI),
    absoluteUri, relativeUri, uriReference, absoluteUriReference,
    uriToString,
    hostname, ipv4address, ipv6address, relSegmentWithColon
    ) where

-- absoluteUri :: Parser Char URI           -- no fragment
-- relativeUri :: Parser Char URI           -- no fragment
-- uriReference :: Parser Char URI          -- optional fragment
-- absoluteUriReference :: Parser Char URI  -- absolute with opt frag
-- uriToString :: URI -> String
-- hostname :: Parser Char String
-- ipv4address :: Parser Char String
-- ipv6address :: Parser Char String

-- Declare imports used from Parser module
import qualified Swish.HaskellUtils.Parse as Parse

-- Originally this was generic in the parsing element,
-- but have now restricted to Char
type CParser b = Parse.Parser Char b

isOneOf :: String -> Char -> Bool
isOneOf       = Parse.isOneOf

isAlpha       :: Char -> Bool
isAlpha       = Parse.isAlpha

isDigit       :: Char -> Bool
isDigit       = Parse.isDigit

isAlphaNum    :: Char -> Bool
isAlphaNum    = Parse.isAlphaNum

isHexDigit    :: Char -> Bool
isHexDigit    = Parse.isHexDigit

parseApply :: CParser b -> (b -> c) -> String -> [(c, String)]
parseApply    = Parse.parseApply

parseReturn :: a -> CParser a
parseReturn   = Parse.parseReturn

parseOne :: CParser b -> CParser b -> String -> [(b, String)]
parseOne      = Parse.parseOne

parseAlt :: CParser b -> CParser b -> String -> [(b, String)]
parseAlt      = Parse.parseAlt

parseSequence :: (Char -> Bool, Char -> Bool) -> String -> [(String, String)]
parseSequence = Parse.parseSequence

-- parseOptional = Parse.parseOptional
-- parseMany     = Parse.parseMany

parseItem :: (Char -> Bool) -> String -> [(Char, String)]
parseItem     = Parse.parseItem

-- catList       = Parse.catList

makeList :: a -> [a]
makeList      = Parse.makeList

(>++>) :: CParser [b] -> CParser [b] -> String -> [([b], String)]
(>++>)        = (Parse.>++>)

(>:>) :: CParser b -> CParser [b] -> String -> [([b], String)]
(>:>)         = (Parse.>:>)

(>*>) :: CParser b -> CParser c -> String -> [((b, c), String)]
(>*>)         = (Parse.>*>)

alphanumCh    :: String
alphanumCh    = Parse.alphanum

-- Generic URI parser
-- Each parser rule is a local function definition,
-- mostly based on the function type:  CParser String
-- e.g. parseURI :: CParser URI

-- Internal data type for URI
-- Components are:
--   scheme authority [segments] query fragment
--   e.g.
--     "http:" "//example.org" ["/","dir/","file"] "?q" "#f"
--     "mailto:" "" ["local@domain"] "" "#f"
--
-- Note: opaque paths are presented as an authority string with
--       empty list of segments and query values.  I think (?) this
--       yields the correct behaviour when working with relative URIs
-- Note: if the final path segment ends with a "/", an empty segment
--       is appended to the segment list.  Thus, the final element
--       of the list corresponds (roughly) to a filename rather than
--       a directory name.
--
data URI = URI String String [String] String String
    deriving ( Eq, Show )
type URIPath = (String,[String],String)

makeAbsUri :: (String, URIPath) -> URI
makeAbsUri   (s,(a,p,q))         = URI s a p q ""

makeRelUri :: URIPath -> URI
makeRelUri   (a,p,q)             = URI "" a p q ""

setFragment :: (URI, String) -> URI
setFragment  (URI s a p q _, f1) = URI s a p q f1

makeHierPath :: ((String, [String]), String) -> URIPath
makeHierPath ((a,p),q)           = (a,p,q)

makeOpaqPath :: String -> URIPath
makeOpaqPath s                   = (s,[],"")

nullURI                          :: URI
nullURI                          = URI "" "" [] "" ""

uriToString :: URI -> String
uriToString (URI sch aut seg qry frag) =
    sch++aut++(foldl (++) [] seg)++qry++frag

-- Local helper functions
nullStr :: String -> [([a], String)]
nullStr  = parseReturn []

alt :: CParser b -> CParser b -> String -> [(b, String)]
alt      = parseAlt

one :: CParser b -> CParser b -> String -> [(b, String)]
one      = parseOne

oneOf :: [CParser b] -> String -> [(b, String)]
oneOf    = foldr1 one

anyOf :: [CParser b] -> String -> [(b, String)]
anyOf    = foldr1 alt

opt :: CParser [a] -> String -> [([a], String)]
opt p    = alt p nullStr

rep :: CParser [b] -> CParser [b]
rep p    = alt ( p >++> rep p ) ( parseReturn [] )

no :: (String -> [a]) -> String -> [([b], String)]
no p inp = if null (p inp) then [([],inp)] else []

-- optG and repG are "greedy" versions of opt and rep
optG :: CParser [a] -> String -> [([a], String)]
optG p   = one p nullStr

repG :: CParser [b] -> CParser [b]
repG p   = one ( p >++> repG p ) ( parseReturn [] )

char :: Char -> String -> [(String, String)]
char c   = cclass (==c)

cclass :: (Char -> Bool) -> String -> [(String, String)]
cclass t = parseItem t `parseApply` makeList

-- Assume that Int is sufficient here
count :: (Int, Int) -> CParser [b] -> String -> [([b], String)]
count (mn,mx) p = countEq mn p >++> countMax (mx-mn) p

countEq :: Int -> CParser [b] -> CParser [b]
countEq n p
    | n > 0     = p >++> countEq (n-1) p
    | otherwise = parseReturn []
                  
countMax :: Int -> CParser [b] -> CParser [b]
countMax n p
    | n > 0     = opt ( p >++> countMax (n-1) p )
    | otherwise = parseReturn []

-- Syntax copied almost verbatim from RFC2396bis:
-- http://www.apache.org/~fielding/uri/rev-2002/rfc2396bis.html

-- Character parsers

-- misc
alpha    :: String -> [(String, String)]
alpha    = cclass isAlpha

alphanum :: String -> [(String, String)]
alphanum = cclass isAlphaNum

alnumhyp :: String -> [(String, String)]
alnumhyp = alt (cclass isAlphaNum) (char '-')

digit    :: String -> [(String, String)]
digit    = cclass isDigit

hexdigit :: String -> [(String, String)]
hexdigit = cclass isHexDigit

dot      :: String -> [(String, String)]
dot      = char '.'

slash    :: String -> [(String, String)]
slash    = char '/'

colon    :: String -> [(String, String)]
colon    = char ':'


-- sect 2
uric :: String -> [(String, String)]
uric = oneOf [ reserved, unreserved, escaped ]

-- sect 3
uricNoSlash :: String -> [(String, String)]
uricNoSlash = oneOf
      [ unreserved,
        char '[',
        char ']',
        char ';',
        char '?',
        char ':',
        char '@',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',',
        escaped  ]
-- sect 2.2
reserved :: String -> [(String, String)]
reserved = oneOf
      [ char '[',
        char ']',
        char ';',
        char '/',
        char '?',
        char ':',
        char '@',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',' ]
-- sect 2.3
unreserved :: String -> [(String, String)]
unreserved = oneOf
      [ cclass isAlpha,
        cclass isDigit,
        mark ]
mark :: String -> [(String, String)]
mark = oneOf
      [ char '-',
        char '_',
        char '.',
        char '!',
        char '~',
        char '*',
        char '\'',
        char '(',
        char ')' ]
-- sect 2.4.1
escaped :: String -> [(String, String)]
escaped = char '%' >++> hexdigit >++> hexdigit
-- sect 3.3
pchar :: String -> [(String, String)]
pchar = oneOf
      [ unreserved,
        char ';',
        char ':',
        char '@',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',',
        escaped  ]

-- URI parser

-- sect 3  (see also section 4.3)
-- absoluteUri -> URI scheme: Path ""
absoluteUri :: CParser URI
absoluteUri = (scheme >++> colon) >*> alt hierPart opaquePart
              `parseApply` makeAbsUri

hierPart :: CParser URIPath
hierPart    = one netPath absPath >*> optG (char '?' >++> query)
              `parseApply` makeHierPath
netPath :: CParser (String,[String])
netPath     = (slash >++> slash >++> authority) >*> optG absPath1
absPath :: CParser (String,[String])
absPath     = nullStr >*> absPath1
absPath1 :: CParser [String]
absPath1    = slash >:> pathSegments

opaquePart :: CParser URIPath
opaquePart  = uricNoSlash >++> repG uric
              `parseApply` makeOpaqPath

-- sect 3.1
scheme :: CParser String
scheme = parseSequence ( isAlpha, isOneOf (alphanumCh++"+-.") )

-- sect 3.2
authority :: CParser String
authority = oneOf [ server, regName, nullStr ]

-- sect 3.2.1
regName :: CParser String
regName = regChar >++> repG regChar

regChar :: String -> [(String, String)]
regChar = oneOf
      [ unreserved,
        char ';',
        char ':',
        char '@',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',',
        escaped ]

-- sect 3.2.2
-- NOTE: blank server option is handled by 'authority' production
server :: CParser String
server      = opt ( userinfo >++> char '@' ) >++> hostport
userinfo    :: String -> [(String, String)]
userinfo    = uinfoCh >++> repG uinfoCh
uinfoCh     :: String -> [(String, String)]
uinfoCh     = oneOf
      [ unreserved,     -- regChar without '@'
        char ';',
        char ':',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',',
        escaped ]
hostport    :: String -> [(String, String)]
hostport    = host >++> optG ( colon >++> port )
host        :: String -> [(String, String)]
host        = oneOf [ ipv6reference, ipv4address, hostname ]
port        :: String -> [(String, String)]
port        = repG digit

hostname :: CParser String
hostname    = domainlabel >++> optG qualified
qualified   :: String -> [(String, String)]
qualified   = rep ( dot >++> domainlabel ) >++>
              opt ( dot >++> toplabel >++> dot >++> no alphanum ) >++>
              no dot   -- force all available domain labels to be taken
domainlabel :: String -> [(String, String)]
domainlabel = alphanum >++> morelabel
toplabel    :: String -> [(String, String)]
toplabel    = alpha    >++> morelabel
morelabel   :: String -> [(String, String)]
morelabel   = optG ( countMax 61 alnumhyp >++> alphanum
                     >++> no alnumhyp )

ipv4address :: String -> [(String, String)]
ipv4address = decoctet >++> dot >++> decoctet
                       >++> dot >++> decoctet
                       >++> dot >++> decoctet
decoctet    :: String -> [(String, String)]
decoctet    = anyOf
      [ digit,
        digit19  >++> digit,
        char '1' >++> digit >++> digit,
        char '2' >++> digit04 >++> digit,
        char '2' >++> char '5' >++> digit05 ]
digit19     :: String -> [(String, String)]
digit19     = cclass (isOneOf "123456789")

-- digit12     :: String -> [(String, String)]
-- digit12     = cclass (isOneOf "12")

digit04     :: String -> [(String, String)]
digit04     = cclass (isOneOf "01234")

digit05     :: String -> [(String, String)]
digit05     = cclass (isOneOf "012345")

ipv6reference :: String -> [(String, String)]
ipv6reference = char '[' >++> ipv6address >++> char ']'
ipv6address :: String -> [(String, String)]
ipv6address = anyOf
      [                         countEq 6 h4c >++> ls32,
                        cc >++> countEq 5 h4c >++> ls32,
        opt h4     >++> cc >++> countEq 4 h4c >++> ls32,
        n_h4c_h4 1 >++> cc >++> countEq 3 h4c >++> ls32,
        n_h4c_h4 2 >++> cc >++> countEq 2 h4c >++> ls32,
        n_h4c_h4 3 >++> cc >++>          h4c  >++> ls32,
        n_h4c_h4 4 >++> cc >++>                    ls32,
        n_h4c_h4 5 >++> cc >++>                    h4,
        n_h4c_h4 5 >++> cc ]
      
h4c :: String -> [(String, String)]
h4c         = h4 >++> colon

n_h4c_h4 :: Int -> String -> [(String, String)]
n_h4c_h4 n  = opt ( countMax n h4c >++> h4 )

cc          :: String -> [(String, String)]
cc          = colon >++> colon
h4          :: String -> [(String, String)]
h4          = count (1,4) hexdigit
ls32          :: String -> [(String, String)]
ls32        = alt (h4c >++> h4) ipv4address

-- sect 3.3
pathSegments :: CParser [String]
pathSegments = one ( (segment >++> slash) >:> pathSegments )
                   ( segment `parseApply` makeList )
segment      :: String -> [(String, String)]
segment      = repG pchar

-- sect 3.4
query :: CParser String
query        = repG ( oneOf [ pchar, slash, char '?' ] )

-- sect 4
uriReference :: CParser URI
uriReference = ( oneOf [ absoluteUri, relativeUri, emptyURI ]
                 >*> optfrag )
               `parseApply` setFragment
               
emptyURI :: CParser URI
emptyURI     = parseReturn nullURI

optfrag      :: String -> [(String, String)]
optfrag      = optG ( char '#' >++> fragment )

absoluteUriReference :: CParser URI
absoluteUriReference = ( absoluteUri >*> optfrag )
                       `parseApply` setFragment

-- sect 4.1
fragment :: CParser String
fragment     = repG ( oneOf [ pchar, slash, char '?' ] )

-- sect 5
relativeUri :: CParser URI
relativeUri  = ( oneOf [ netPath, absPath, relPath, nulPath ] >*>
                 optG (char '?' >++> query)
                `parseApply` makeHierPath )
              `parseApply` makeRelUri

nulPath :: CParser (String,[String])
nulPath     = parseReturn ("",[])

relPath :: CParser (String,[String])
-- relPath      = nullStr >*> (relSegment >:> optG absPath1)
relPath      = nullStr >*> one ( relSegment >++> slash >:> pathSegments )
                               ( relSegment `parseApply` makeList )
                               -- [[[TODO: factor higher order function
                               -- for relPath and pathSegments]]]

relSegment   :: CParser String
relSegment   = relSegCh >++> repG relSegCh
relSegCh     :: String -> [(String, String)]
relSegCh     = oneOf
      [ unreserved,     -- pchar without ':'
        char ';',
        char '@',
        char '&',
        char '=',
        char '+',
        char '$',
        char ',',
        escaped  ]

relSegmentWithColon :: CParser String
relSegmentWithColon = relSegment >++> colon >++> segment

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, G. KLYNE.  All rights reserved.
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
-- $Source: /file/cvsdev/HaskellUtils/ParseURI.hs,v $
-- $Author: graham $
-- $Revision: 1.1 $
-- $Log: ParseURI.hs,v $
-- Revision 1.1  2004/01/13 12:31:24  graham
-- Move modules from HaskellRDF to HaskellUtils project
--
-- Revision 1.11  2003/09/24 18:50:52  graham
-- Revised module format to be Haddock compatible.
--
-- Revision 1.10  2003/06/03 19:24:13  graham
-- Updated all source modules to cite GNU Public Licence
--
-- Revision 1.9  2003/05/20 23:35:28  graham
-- Modified code to compile with GHC hierarchical libraries
--
-- Revision 1.8  2003/03/05 22:16:23  graham
-- URI code passes all unit tests
--
-- Revision 1.7  2003/03/05 14:47:45  graham
-- Relative URI code complete, not tested
-- Fixed a URI parser bug
--
-- Revision 1.6  2003/02/27 20:29:53  graham
-- Fixed some more parser bugs.
-- All parser tests pass.
-- QName and relative path handling to do.
--
-- Revision 1.5  2003/02/27 18:48:05  graham
-- Fix URI parser bug.
-- Add more URI parser test cases.
--
-- Revision 1.4  2003/02/27 15:28:45  graham
-- Updated internal structure of parsed URI.
-- Passes parser unit tests
--
-- Revision 1.3  2003/02/27 13:54:30  graham
-- ParseURI module passes unit test
--
-- Revision 1.2  2003/02/27 09:50:25  graham
-- Add URI parser test cases, some name changes
--
-- Revision 1.1  2003/02/27 08:59:53  graham
-- Separate URI parser from main URI module
--
