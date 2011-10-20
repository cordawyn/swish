{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.Geo
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines some commonly used vocabulary terms from the Geo
--  vocabulary (<http://www.w3.org/2003/01/geo/wgs84_pos#>).
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.Geo
    ( 
      namespaceGEO
      , geoLat
      , geoLong
    )
where

import Swish.Utils.Namespace (Namespace, makeNamespace, ScopedName, makeNSScopedName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

import qualified Data.Text as T

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

geoURI :: URI
geoURI = fromMaybe (error "Internal error processing Geo URI") $ parseURI "http://www.w3.org/2003/01/geo/wgs84_pos#"

-- | Maps @geo@ to <http://www.w3.org/2003/01/geo/wgs84_pos#>.
namespaceGEO :: Namespace
namespaceGEO = makeNamespace (Just "geo") geoURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toG :: T.Text -> ScopedName
toG  = makeNSScopedName namespaceGEO

-- | @geo:lat@.
geoLat :: ScopedName
geoLat = toG "lat"

-- | @geo:long@.
geoLong :: ScopedName
geoLong = toG "long"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 Douglas Burke
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
