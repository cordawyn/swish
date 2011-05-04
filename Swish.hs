--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  Swish:  Semantic Web Inference Scripting in Haskell
--
--  This program is a simple skeleton for constructing Semantic Web [1]
--  inference tools in Haskell, using the RDF graph and several RDF
--  parsers (at present Notation 3 and NTriples).
--
--  It might be viewed as a kind of embroyonic CWM [2] in Haskell,
--  except that the intent is that Haskell will be used as a primary
--  language for defining inferences.  As such, Swish is an open-ended
--  toolkit for constructing new special-purpose Semantic Web
--  applications rather than a closed, self-contained general-purpose
--  SW application.  As such, it is part of another experiment along
--  the lines described in [3].
--
--  The script format used by Swish is described in
--  "Swish.RDF.SwishScript".
--
--  Users wishing to process RDF data directly may prefer to look at
--  the following modules; "Swish.RDF", "Swish.RDF.N3Parser",
--  "Swish.RDF.N3Formatter", "Swish.RDF.NTParser" and
--  "Swish.RDF.NTFormatter".
--
--  (1) Semantic web: <http://www.w3.org/2001/sw/>
--
--  (2) CWM:          <http://www.w3.org/2000/10/swap/doc/cwm.html>
--
--  (3) Motivation:   <http://www.w3.org/2000/10/swap/doc/Motivation.html>
--
--  (4) Notation 3:   <http://www.w3.org/TeamSubmission/2008/SUBM-n3-20080114/>
--
--  (5) RDF:          <http://www.w3.org/RDF/>
--
--------------------------------------------------------------------------------

module Swish
       (
         module Swish.RDF.SwishMain
       ) where

import Swish.RDF.SwishMain
-- should this re-export Swish.RDF.SwishMain or other components?
       
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
