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
--  This module is now a thin wrapper on top of "Network.URI". Some of the tests
--  now fail but it is not entirely clear that these tests are still valid.
--
--  See RFC3986 [1], which obsoletes RFC 2396 [2]. There is also the RFC
--  for IRIs, but that is ignored here.
--
--  [1] <http://www.ietf.org/rfc/rfc3896.txt>
--
--  [2] <http://www.ietf.org/rfc/rfc2396.txt>
--
--------------------------------------------------------------------------------

module Swish.HaskellUtils.ProcessURI
       (isAbsoluteURIRef, isValidURIRef, absoluteUriPart)
       where

import qualified Network.URI as N

import Data.Maybe (fromJust)

{-
Network.URI does not quite match the behavior of the original
code (some of the tests in URITest no longer pass), but it's not
clear that those tests are actually valid (perhaps due to changes
in expected behavior and the standards, such as RFC 3986.
-}

-- | Test supplied string for valid URI syntax.
--
-- Please use 'Network.URI.usURIReference' instead.
isValidURIRef :: String -> Bool
isValidURIRef = N.isURIReference

-- | Test supplied string for valid absolute URI reference syntax.
-- 
-- Please use 'Network.URI.isURI' instead.
isAbsoluteURIRef :: String -> Bool
isAbsoluteURIRef = N.isURI

-- | Get absolute URI given base and relative reference.
-- 
-- Please use 'Network.URI.relativeTo' and 'N.parseURI'/'N.parseURIReference'
-- instead.
absoluteUriPart :: String -- ^ URI base
                   -> String -- ^ URI reference
                   -> String
absoluteUriPart base rel = showURI $ fromJust $ N.relativeTo (fromJust (N.parseURIReference rel)) (fromJust (N.parseURI base))
  
showURI :: N.URI -> String
showURI u = N.uriToString id u ""
    
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
