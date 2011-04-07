--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  BuiltInDatatypes
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module collects references and provides access to all of the
--  datatypes built in to Swish.
--
--------------------------------------------------------------------------------

module Swish.RDF.BuiltInDatatypes
    ( allDatatypes, findRDFDatatype )
where

import Swish.RDF.RDFDatatype
    ( RDFDatatype
    )

import Swish.Utils.LookupMap
    ( LookupMap(..), mapFindMaybe
    )

import Swish.Utils.Namespace
    ( ScopedName(..) )

import Swish.RDF.RDFDatatypeXsdString
    ( rdfDatatypeXsdString )

import Swish.RDF.RDFDatatypeXsdInteger
    ( rdfDatatypeXsdInteger )

------------------------------------------------------------
--  Declare datatype map
------------------------------------------------------------

allDatatypes :: [RDFDatatype]
allDatatypes =
    [ rdfDatatypeXsdString
    , rdfDatatypeXsdInteger
    ]

findRDFDatatype :: ScopedName -> Maybe RDFDatatype
findRDFDatatype nam = mapFindMaybe nam (LookupMap allDatatypes)

------------------------------------------------------------
--  Declare datatype subtypes map
------------------------------------------------------------

{-
allDatatypeSubtypes :: [xxx]
allDatatypeSubtypes = []
--  [[[details TBD]]]
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
