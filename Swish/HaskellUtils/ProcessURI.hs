--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ProcessURI
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines a collection of functions for manipulating URIs.
--
--  Functions provided deal with:
--
--  * Validating and parsing URI syntax
--
--  * Separating a fragment from a URI
--
--  * Separating URI into QName and local name
--
--  * Relative URI computations
--
--  The primary reference for URI handling is RFC2396 [1],
--  as updated by RFC 2732 [2].
--  RFC 1808 [3] contains a number of test cases for relative URI handling.
--  Dan Connolly's Python module @uripath.py@ [4] also contains useful details
--  and test cases.
--
--  [1] <http://www.ietf.org/rfc/rfc2396.txt>
--
--  [2] <http://www.ietf.org/rfc/rfc2732.txt>
--
--  [3] <http://www.ietf.org/rfc/rfc1808.txt>
--
--  [4] <http://www.w3.org/2000/10/swap/uripath.py>
--
--------------------------------------------------------------------------------

-- the only routines used are isAbsoluteURIRef, isValidURIRef and absoluteUriPart

module Swish.HaskellUtils.ProcessURI
    ( URI(URI)
    , isValidURIRef, isAbsoluteURIRef, isAbsoluteURI
    , parseURIRef, parseAbsoluteURIRef, parseAbsoluteURI
    , getURIRef, normalizeURI, compareURI, normSeg, normSeg1, mergeSeg
    , splitURIFragment, makeURIWithFragment -- , splitQName
    , relativeRefPart, absoluteUriPart
    )
where

import Swish.HaskellUtils.ParseURI
import Swish.HaskellUtils.Parse
-- type Parser a b = Parse.Parser a b
-- isValid         = Parse.isValid
-- parseApply      = Parse.parseApply

import qualified Network.URI as N
-- import qualified Text.URI as TN

import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import Data.List.Split (split, keepDelimsR, oneOf)

{-
Hmm, both Network.URI and Text.URI do not quite match the
existing behavior.
-}

-- Test supplied string for valid URI syntax
isValidURIRef :: String -> Bool
-- isValidURIRef = isJust . TN.parseURI
isValidURIRef = N.isURIReference
-- isValidURIRef = isValid parseURIRef

-- Test supplied string for valid absolute URI reference syntax
isAbsoluteURIRef :: String -> Bool
-- isAbsoluteURIRef = fromMaybe False . fmap (not . TN.isRelative) . TN.parseURI
isAbsoluteURIRef = N.isURI
-- isAbsoluteURIRef = isValid parseAbsoluteURIRef

-- Test supplied string for valid absolute URI syntax
isAbsoluteURI :: String -> Bool
isAbsoluteURI = isValid parseAbsoluteURI

-- URI parser (see Parse and ParseURI modules)
parseURIRef :: Parser Char String
parseURIRef = uriReference `parseApply` uriToString

-- Absolute URI reference parser (see Parse module)
parseAbsoluteURIRef :: Parser Char String
parseAbsoluteURIRef = absoluteUriReference `parseApply` uriToString

-- Absolute URI parser (see Parse module)
parseAbsoluteURI :: Parser Char String
parseAbsoluteURI = absoluteUri `parseApply` uriToString

-- Parse and return URI reference as URI value
getURIRef :: String -> URI
getURIRef s = extractURIRef (uriReference s)
    where
    extractURIRef [(u,"")] = u
    extractURIRef [(_,_)]  = URI "" "" ["<invalid URI>"] "" ""
    extractURIRef _        = URI "" "" ["<ambiguous URI>"] "" ""

-- Normalize URI string
normalizeURI :: String -> String
normalizeURI str = uriToString (URI sc au (normSeg se) qu fr)
    where
    URI sc au se qu fr = getURIRef str

-- Compare two URIs
-- Takes account of normalizations that can be applied to all URIs
-- (2003-02-20, currently subject to W3C TAG debate)
compareURI :: String -> String -> Bool
compareURI = (==)

-- Separate URI-with-fragment into URI and fragment ID
splitURIFragment :: String -> ( String, Maybe String )
    -- splitURIFragment "http://example.org/aaa#bbb" =
    --     ("http://example.org/aaa",Just "bbb")
    -- splitURIFragment "http://example.org/aaa" =
    --     ("http://example.org/aaa",Nothing)
splitURIFragment inp =
    case uriReference inp of
        [(URI s a p q f,"")] -> (uriToString (URI s a p q ""),pickFrag f)
        _ -> error ("splitURIFragment, Invalid URI: "++inp)
    where
        pickFrag ('#':f) = Just f
        pickFrag _       = Nothing

-- Construct URI-with-fragment using URI and supplied fragment id
makeURIWithFragment :: String -> Maybe String -> String
    -- makeURIWithFragment "http://example.org/aaa" (Just "fff") =
    --     "http://example.org/aaa#fff"
    -- makeURIWithFragment "http://example.org/aaa#bbb" (Just "fff") =
    --     "http://example.org/aaa#fff"
    -- makeURIWithFragment "http://example.org/aaa" Nothing
    --     "http://example.org/aaa"
    -- makeURIWithFragment "http://example.org/aaa#bbb" Nothing
    --     "http://example.org/aaa"
makeURIWithFragment base frag =
    case frag of
        Just f  -> b ++ "#" ++ f
        Nothing -> b
        where
            (b,_) = splitURIFragment base

{-
-- Separate URI into QName URI and local name
splitQName :: String -> ( String, String )
    -- splitQname "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
    -- splitQname "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
    -- splitQname "http://example.org/aaa/"    = ("http://example.org/aaa/","")
splitQName qn = splitAt (scanQName qn (-1) 0) qn

-- helper function for splitQName
-- Takes 3 arguments:
--   QName to scan
--   index of last name-start char, or (-1)
--   number of characters scanned so far
-- Returns index of start of name, or length of list
--
scanQName :: String -> Int -> Int -> Int
scanQName (nextch:more) (-1) nc
    | isNameStartChar nextch  = scanQName more nc   (nc+1)
    | otherwise               = scanQName more (-1) (nc+1)
scanQName (nextch:more) ns nc
    | not (isNameChar nextch) = scanQName more (-1) (nc+1)
    | otherwise               = scanQName more ns   (nc+1)
scanQName "" (-1) nc = nc
scanQName "" ns   _  = ns

-- Definitions here per XML namespaces, NCName production,
-- restricted to characters used in URIs.
-- cf. http://www.w3.org/TR/REC-xml-names/
isNameStartChar c = isAlpha c || c == '_'
isNameChar      c = isAlpha c || isDigit c || c `elem` ".-_"

-}

-- Get reference relative to given base
relativeRefPart :: String -> String -> String
    -- relativeRefPart "base:" "base:relativeRef" = "relativeRef"
    -- relativeRefPart "base:" "another:URI"      = "another:URI"
relativeRefPart base full =
    uriToString ( relPartRef ( getURIRef base ) ( getURIRef full ) )
    where
    relPartRef (URI sc1 au1 se1 _ _) ( URI sc2 au2 se2 qu2 fr2 )
        | sc1 /= sc2 = URI sc2 au2 (normSeg se2) qu2 fr2    -- different schemes
        | opaque au1 = URI "" au2 (normSeg se2) qu2 fr2     -- same scheme, base is opaque
        | au1 /= au2 = URI "" au2 (normSeg se2) qu2 fr2     -- same scheme, different authority
        | otherwise  = URI "" "" (relPath (normSeg se1) (normSeg se2) ) qu2 fr2
    -- If paths share a leading segment (other than "/") then compute a path relative
    -- to the base URI, otherwise return a root-relative path
    relPath _  []    = ["/"]
    relPath [] s2    = s2
    relPath ("/":s1h:s1t) s2@("/":s2h:s2t)
        | s1h == s2h = relPartSeg s1t s2t
        | otherwise  = s2
    relPath (s1h:s1t) s2@(s2h:s2t)
        | s1h == s2h = relPartSeg s1t s2t
        | otherwise  = s2
    {- relPath s1@(_:_) s2@(_:_) = relPartSeg s1 s2 [[[REDUNDANT?]]] -}
    -- Path-segment relative to base:
    -- (An alternative would be descendent relative to base, otherwise relative to root)
    --   relPartSeg a/b a/c -> c        (case 1:  common leading segments)
    --   relPartSeg a/b a/  -> ./       (case 1a: identical paths with empty name)
    --   relPartSeg a   b/c -> b/c      (case 2:  all base path segments used)
    --   relPartSeg b   c   -> c        (case 2)
    --   relPartSeg a   c/  -> c/       (case 2)
    --   relPartSeg ""  c   -> c        (case 2)
    --   relPartSeg a ""    -> ""       (case 2)
    --   relPartSeg a   c:d -> ./c:d    (case 2a: bare name looks like URI
    --   relPartSeg a/b c   -> ../c     (case 3: unused base path segments)
    --   relPartSeg a/b ""  -> ../      (case 3)
    --   relPartSeg a/  c   -> ../c     (case 3)
    --   relPartSeg a/  ""  -> ../      (case 3)
    -- NOTE the last element of the path segment lists are always the "name" component,
    -- and is present as an empty string if the path ends with a '/' character
    relPartSeg [_] [""]    = ["./",""]                  -- Case 1a
    relPartSeg [_] [st]
        | looksLikeURI st  = ["./",st]                  -- Case 2a
        | otherwise        = [st]                       -- Case 2
    relPartSeg [_] s2      = s2                         -- Case 2
    relPartSeg s1  [s2t]   = difPartSeg s1 [s2t]        -- Case 3  (this test should be redundant)
    relPartSeg s1@(s1h:s1t) s2@(s2h:s2t)
        | s1h == s2h = relPartSeg s1t s2t               -- Case 1
        | otherwise  = difPartSeg s1 s2                 -- Case 2 or 3 ...
                       
    -- missing patterns for relPartSeg according to ghc:
    --  Patterns not matched:
    --     [] []
    --     [] (_ : (_ : _))
    --     (_ : (_ : _)) []
    --
                       
    difPartSeg [_]     s2  = s2                         -- Case 2  (final base segment is ignored)
    difPartSeg (_:s1t) s2  = "../" : difPartSeg s1t s2  -- Case 3

    -- missing patterns for difPartSeg according to ghc:
    --  Patterns not matched:
    --     [] _

-- Get absolute URI given base and relative reference
-- NOTE:  absoluteURI base (relativeRef base u) is always equivalent to u.
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html
absoluteUriPart :: String -> String -> String
    -- absoluteUriPart "base:" "relativeRef" = "base:relativeRef"
    -- absoluteUriPart "base:" "another:URI" = "another:URI"

-- absoluteUriPart = TN.mergeURIStrings

absoluteUriPart base = showURI . absoluteUriPart' base 
-- absoluteUriPart base rel = showURI $ fromJust $ N.relativeTo (fromJust (N.parseURIReference rel)) (fromJust (N.parseURI base))
  
{-
Try to implement the algorithm of section 5.2
from http://www.ietf.org/rfc/rfc3986.txt

5.2.1.  Pre-parse the Base URI

   The base URI (Base) is established according to the procedure of
   Section 5.1 and parsed into the five main components described in
   Section 3.  Note that only the scheme component is required to be
   present in a base URI; the other components may be empty or
   undefined.  A component is undefined if its associated delimiter does
   not appear in the URI reference; the path component is never
   undefined, though it may be empty.

   Normalization of the base URI, as described in Sections 6.2.2 and
   6.2.3, is optional.  A URI reference must be transformed to its
   target URI before it can be normalized.

5.2.2.  Transform References

   For each URI reference (R), the following pseudocode describes an
   algorithm for transforming R into its target URI (T):

      -- The URI reference is parsed into the five URI components
      --
      (R.scheme, R.authority, R.path, R.query, R.fragment) = parse(R);

      -- A non-strict parser may ignore a scheme in the reference
      -- if it is identical to the base URI's scheme.
      --
      if ((not strict) and (R.scheme == Base.scheme)) then
         undefine(R.scheme);
      endif;

      if defined(R.scheme) then
         T.scheme    = R.scheme;
         T.authority = R.authority;
         T.path      = remove_dot_segments(R.path);
         T.query     = R.query;
      else
         if defined(R.authority) then
            T.authority = R.authority;
            T.path      = remove_dot_segments(R.path);
            T.query     = R.query;
         else
            if (R.path == "") then
               T.path = Base.path;
               if defined(R.query) then
                  T.query = R.query;
               else
                  T.query = Base.query;
               endif;
            else
               if (R.path starts-with "/") then
                  T.path = remove_dot_segments(R.path);
               else
                  T.path = merge(Base.path, R.path);
                  T.path = remove_dot_segments(T.path);
               endif;
               T.query = R.query;
            endif;
            T.authority = Base.authority;
         endif;
         T.scheme = Base.scheme;
      endif;

      T.fragment = R.fragment;

5.2.3.  Merge Paths

   The pseudocode above refers to a "merge" routine for merging a
   relative-path reference with the path of the base URI.  This is
   accomplished as follows:
   
o  If the base URI has a defined authority component and an empty
      path, then return a string consisting of "/" concatenated with the
      reference's path; otherwise,

   o  return a string consisting of the reference's path component
      appended to all but the last segment of the base URI's path (i.e.,
      excluding any characters after the right-most "/" in the base URI
      path, or excluding the entire base URI path if it does not contain
      any "/" characters).

5.2.4.  Remove Dot Segments

   The pseudocode also refers to a "remove_dot_segments" routine for
   interpreting and removing the special "." and ".." complete path
   segments from a referenced path.  This is done after the path is
   extracted from a reference, whether or not the path was relative, in
   order to remove any invalid or extraneous dot-segments prior to
   forming the target URI.  Although there are many ways to accomplish
   this removal process, we describe a simple method using two string
   buffers.

   1.  The input buffer is initialized with the now-appended path
       components and the output buffer is initialized to the empty
       string.

   2.  While the input buffer is not empty, loop as follows:

       A.  If the input buffer begins with a prefix of "../" or "./",
           then remove that prefix from the input buffer; otherwise,

       B.  if the input buffer begins with a prefix of "/./" or "/.",
           where "." is a complete path segment, then replace that
           prefix with "/" in the input buffer; otherwise,

       C.  if the input buffer begins with a prefix of "/../" or "/..",
           where ".." is a complete path segment, then replace that
           prefix with "/" in the input buffer and remove the last
           segment and its preceding "/" (if any) from the output
           buffer; otherwise,

       D.  if the input buffer consists only of "." or "..", then remove
           that from the input buffer; otherwise,

       E.  move the first path segment in the input buffer to the end of
           the output buffer, including the initial "/" character (if
           any) and any subsequent characters up to, but not including,
           the next "/" character or the end of the input buffer.

   3.  Finally, the output buffer is returned as the result of
       remove_dot_segments.

   Note that dot-segments are intended for use in URI references to
   express an identifier relative to the hierarchy of names in the base
   URI.  The remove_dot_segments algorithm respects that hierarchy by
   removing extra dot-segments rather than treat them as an error or
   leaving them to be misinterpreted by dereference implementations.

   The following illustrates how the above steps are applied for two
   examples of merged paths, showing the state of the two buffers after
   each step.

      STEP   OUTPUT BUFFER         INPUT BUFFER

       1 :                         /a/b/c/./../../g
       2E:   /a                    /b/c/./../../g
       2E:   /a/b                  /c/./../../g
       2E:   /a/b/c                /./../../g
       2B:   /a/b/c                /../../g
       2C:   /a/b                  /../g
       2C:   /a                    /g
       2E:   /a/g

      STEP   OUTPUT BUFFER         INPUT BUFFER

       1 :                         mid/content=5/../6
       2E:   mid                   /content=5/../6
       2E:   mid/content=5         /../6
       2C:   mid                   /6
       2E:   mid/6

   Some applications may find it more efficient to implement the
   remove_dot_segments algorithm by using two segment stacks rather than
   strings.

      Note: Beware that some older, erroneous implementations will fail
      to separate a reference's query component from its path component
      prior to merging the base and reference paths, resulting in an
      interoperability failure if the query component contains the
      strings "/../" or "/./".

5.3.  Component Recomposition

   Parsed URI components can be recomposed to obtain the corresponding
   URI reference string.  Using pseudocode, this would be:

      result = ""

      if defined(scheme) then
         append scheme to result;
         append ":" to result;
      endif;

      if defined(authority) then
         append "//" to result;
         append authority to result;
      endif;

      append path to result;

      if defined(query) then
         append "?" to result;
         append query to result;
      endif;

      if defined(fragment) then
         append "#" to result;
         append fragment to result;
      endif;

      return result;

   Note that we are careful to preserve the distinction between a
   component that is undefined, meaning that its separator was not
   present in the reference, and a component that is empty, meaning that
   the separator was present and was immediately followed by the next
   component separator or the end of the reference.

5.4.  Reference Resolution Examples

   Within a representation with a well defined base URI of

      http://a/b/c/d;p?q

   a relative reference is transformed to its target URI as follows.

5.4.1.  Normal Examples

      "g:h"           =  "g:h"
      "g"             =  "http://a/b/c/g"
      "./g"           =  "http://a/b/c/g"
      "g/"            =  "http://a/b/c/g/"
      "/g"            =  "http://a/g"
      "//g"           =  "http://g"
      "?y"            =  "http://a/b/c/d;p?y"
      "g?y"           =  "http://a/b/c/g?y"
      "#s"            =  "http://a/b/c/d;p?q#s"
      "g#s"           =  "http://a/b/c/g#s"
      "g?y#s"         =  "http://a/b/c/g?y#s"
      ";x"            =  "http://a/b/c/;x"
      "g;x"           =  "http://a/b/c/g;x"
      "g;x?y#s"       =  "http://a/b/c/g;x?y#s"
      ""              =  "http://a/b/c/d;p?q"
      "."             =  "http://a/b/c/"
      "./"            =  "http://a/b/c/"
      ".."            =  "http://a/b/"
      "../"           =  "http://a/b/"
      "../g"          =  "http://a/b/g"
      "../.."         =  "http://a/"
      "../../"        =  "http://a/"
      "../../g"       =  "http://a/g"

5.4.2.  Abnormal Examples

   Although the following abnormal examples are unlikely to occur in
   normal practice, all URI parsers should be capable of resolving them
   consistently.  Each example uses the same base as that above.

   Parsers must be careful in handling cases where there are more ".."
   segments in a relative-path reference than there are hierarchical
   levels in the base URI's path.  Note that the ".." syntax cannot be
   used to change the authority component of a URI.

      "../../../g"    =  "http://a/g"
      "../../../../g" =  "http://a/g"

   Similarly, parsers must remove the dot-segments "." and ".." when
   they are complete components of a path, but not when they are only
   part of a segment.

      "/./g"          =  "http://a/g"
      "/../g"         =  "http://a/g"
      "g."            =  "http://a/b/c/g."
      ".g"            =  "http://a/b/c/.g"
      "g.."           =  "http://a/b/c/g.."
      "..g"           =  "http://a/b/c/..g"

   Less likely are cases where the relative reference uses unnecessary
   or nonsensical forms of the "." and ".." complete path segments.

      "./../g"        =  "http://a/b/g"
      "./g/."         =  "http://a/b/c/g/"
      "g/./h"         =  "http://a/b/c/g/h"
      "g/../h"        =  "http://a/b/c/h"
      "g;x=1/./y"     =  "http://a/b/c/g;x=1/y"
      "g;x=1/../y"    =  "http://a/b/c/y"

   Some applications fail to separate the reference's query and/or
   fragment components from the path component before merging it with
   the base path and removing dot-segments.  This error is rarely
   noticed, as typical usage of a fragment never includes the hierarchy
   ("/") character and the query component is not normally used within
   relative references.

      "g?y/./x"       =  "http://a/b/c/g?y/./x"
      "g?y/../x"      =  "http://a/b/c/g?y/../x"
      "g#s/./x"       =  "http://a/b/c/g#s/./x"
      "g#s/../x"      =  "http://a/b/c/g#s/../x"

   Some parsers allow the scheme name to be present in a relative
   reference if it is the same as the base URI scheme.  This is
   considered to be a loophole in prior specifications of partial URI
   [RFC1630].  Its use should be avoided but is allowed for backward
   compatibility.

      "http:g"        =  "http:g"         ; for strict parsers
                      /  "http://a/b/c/g" ; for backward compatibility

-}

-- it looks like N.relativeTo isn't consistent with RFC 3986
-- or at least it doesn't pass the tests from URITest

splitPath :: String -> [String]
splitPath = (split . keepDelimsR . oneOf) "/"

removeDotSegments :: String -> String
removeDotSegments p =
  let ibuff = splitPath p
  in concat $ reverse $ cp ibuff []
    where
      cp [] obuff = obuff
      cp ["."] obuff = obuff
      cp [".."] [] = []
      cp [".."] ["/"] = ["/"]
      cp [".."] (_:obuff) = obuff
      cp ("./":is) obuff = cp is obuff
      cp ("../":is) [] = cp is []
      cp ("../":is) ["/"] = cp is ["/"]
      cp ("../":is) (_:obuff) = cp is obuff
      cp (i:is) obuff = cp is (i : obuff)
      
-- QUS: does an empty path include "/"
mergePath :: Maybe N.URIAuth -> String -> String -> String
mergePath bauth bpath rpath =
  if isJust bauth && (null bpath || bpath == "/")
  then "/" ++ rpath
  else concat (init (splitPath bpath)) ++ rpath -- rely on splitPath never returning an empty list
    
showURI :: N.URI -> String
showURI u = N.uriToString id u ""
-- showURI (N.URI s a p q f) = N.uriToString id (N.URI s a (removeDotSegments p) q f) ""
    
-- try a strict parser
absoluteUriPart' :: String -> String -> N.URI
absoluteUriPart' base rel = 
  let 
    -- we assume that base and rel are valid (URI and URI reference respectively)
    Just (N.URI bscheme bauth bpath bquery _)     = N.parseURI base
    Just (N.URI rscheme rauth rpath rquery rfrag) = N.parseURIReference rel

    rds = removeDotSegments
    turi = if null rscheme 
           then let hd = N.URI bscheme
                in if isJust rauth
                   then hd rauth (rds rpath) rquery
                   else let hd2 = hd bauth
                        in case rpath of
                          []      -> hd2 bpath (if null rquery then bquery else rquery) 
                          ('/':_) -> hd2 (rds rpath) rquery
                          _       -> hd2 (rds (mergePath bauth bpath rpath)) rquery
                    
           else N.URI rscheme rauth (rds rpath) rquery

  in turi rfrag
         
{-

absoluteUriPart base rel =
    uriToString ( joinRef ( getURIRef base ) ( getURIRef rel ) )
    where
    joinRef (URI sc1 au1 se1 _ _) u2@( URI sc2 au2 se2 qu2 fr2 )
        -- non-validating case here?  (See RFC2396bis section 5.2)
        | sc2 /= ""     = u2
        | opaque au1    = URI sc1 au2 se2 qu2 fr2                       -- Base not relative
        | au2 /= ""     = URI sc1 au2 se2 qu2 fr2
        | se2 == []     = if qu2 == "" then URI "" "" [] "" fr2         -- Same document
                                       else URI sc1 au1 se1 qu2 fr2     -- Base document
        | otherwise     = URI sc1 au1 ( mergeSeg se1 se2 ) qu2 fr2
-}

-- Test authority string for opaque form (non-null and not starting with '/')
opaque :: String -> Bool
opaque ""      = False
opaque ('/':_) = False
opaque _       = True

-- Merge segment se2 with base segment se1
mergeSeg :: [String] -> [String] -> [String]
mergeSeg _ s2@("/":_) = normSeg s2
mergeSeg [] se2       = normSeg ("/":se2)
mergeSeg se1 se2      = normSeg ( init se1 ++ se2 )

-- Normalize ./ and ../ in segment list:
-- Don't touch leading "/"
-- Don't allow "../" to cancel another "../"
-- Leave bare "./"
-- Remove any trailing "./"
normSeg :: [String] -> [String]
normSeg ("/":st)      = "/" : normSeg1 st
normSeg st            = normSeg1 st

normSeg1 :: [String] -> [String]
normSeg1 []           = []
normSeg1 ["./"]       = ["./"]
normSeg1 ["."]        = ["./",""]                      -- trailing '.' is treated as './'
normSeg1 [".."]       = ["../",""]                     -- trailing '..' is treated as '../'
normSeg1 p@["./",st]
    | looksLikeURI st = p
    | otherwise       = [st]
normSeg1 ("./":st)    = normSeg1 st
normSeg1 (s1:st)      = normSeg2 (s1: normSeg1 st)    -- TEST CASE:  a/b/../../c

normSeg2 :: [String] -> [String]
normSeg2 s@("../":"../":_) = s
normSeg2 ["./","../"] = ["./"]
normSeg2 (_:"../":st) = st
normSeg2 [sh,"./"]    = [sh]
normSeg2 (sh:"./":st) = sh:st
normSeg2 ("./":st)    = st
normSeg2 st           = st

-- Test if string looks like a URI, by virtue of starting with a 'name:'
looksLikeURI :: String -> Bool
looksLikeURI name = not ( null ( relSegmentWithColon name ) )

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
--  Foobar is distributed in the hope that it will be useful,
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
