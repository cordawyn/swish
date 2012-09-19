{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Ord
--  Copyright   :  (c) 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP
--
--  Provide an ordering for URIs (that is, an 'Ord' instance for
--  'URI').
--
--  This instance is provided so that URIs can be used as the
--  key of a 'Data.Map.Map'. Case is relevant for the ordering,
--  and no attempt is made to decode percent-encoded values (i.e.
--  the comparison does /not/ use a canonical or normalized form).
--
--  For @network@ version @2.4.0.0@ and higher, this module is a no-op,
--  since 'Network.URI' now defines these instances.
--
--------------------------------------------------------------------------------

module Network.URI.Ord () where

#if MIN_VERSION_network(2,4,0)

#else

import Network.URI (URI(..), URIAuth(..))

-- NOTE: we do not say compare = comparing show for the URI
-- instance since the standard show instance for URIs does not
-- include a password if included as part of the authority field.

instance Ord URI where
    URI s1 a1 o1 q1 f1 `compare` URI s2 a2 o2 q2 f2 =
        (s1,a1,o1,q1,f1) `compare` (s2,a2,o2,q2,f2)

instance Ord URIAuth where
    URIAuth ui1 rn1 p1 `compare` URIAuth ui2 rn2 p2 =
        (ui1,rn1,p1) `compare` (ui2,rn2,p2)

#endif

--------------------------------------------------------------------------------
--
--  Copyright (c) 2012 Douglas Burke
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
