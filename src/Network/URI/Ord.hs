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
--  Portability :  H98
--
--  Provide an ordering for URIs.
--
--------------------------------------------------------------------------------

module Network.URI.Ord () where

import Data.Ord (comparing)

import Network.URI (URI)

-- | This instance is provided so that URIs can be used as the
--   key of a 'Data.Map.Map'.

instance Ord URI where
    compare = comparing show -- TODO: do this properly

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
