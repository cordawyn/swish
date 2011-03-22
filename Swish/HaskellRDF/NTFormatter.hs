--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3Formatter
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module implements a NTriples formatter (see [1])
--  for an RDFGraph value.
--
--
-- REFERENCES:
--
-- 1 <http://www.w3.org/TR/rdf-testcases/#ntriples>
--     RDF Test Cases
--     W3C Recommendation 10 February 2004
--
--------------------------------------------------------------------------------

module Swish.HaskellRDF.NTFormatter
    ( NodeGenLookupMap
    , formatGraphAsString
    , formatGraphAsShowS
    )
where

import Swish.HaskellRDF.RDFGraph (
  RDFGraph, RDFLabel(..),
  getArcs, 
  )

import Swish.HaskellRDF.GraphClass
    ( Arc(..) )

import Swish.HaskellUtils.LookupMap
    ( LookupMap, emptyLookupMap
    , mapFind, mapAdd
    )

import Data.Char (isDigit)

-- import "mtl" Control.Monad.State
import Control.Monad.State

----------------------------------------------------------------------
--  Graph formatting state monad
----------------------------------------------------------------------
--
--  This is a lot simpler than other formatters.

--  | Node name generation state information that carries through
--  and is updated by nested formulae
type NodeGenLookupMap = LookupMap (RDFLabel,Int)

data NTFormatterState = NTFS { 
      ntfsNodeMap :: NodeGenLookupMap,
      ntfsNodeGen :: Int
    } deriving Show

emptyNTFS :: NTFormatterState
emptyNTFS = NTFS {
              ntfsNodeMap = emptyLookupMap,
              ntfsNodeGen = 0
              }

type Formatter a = State NTFormatterState a

----------------------------------------------------------------------
--  Define a top-level formatter function:
--  accepts a graph and returns a string
----------------------------------------------------------------------

formatGraphAsString :: RDFGraph -> String
formatGraphAsString gr = formatGraphAsShowS gr "\n"

formatGraphAsShowS :: RDFGraph -> ShowS
formatGraphAsShowS gr = 
    let (out, _, _) = formatGraphInternal gr
    in out 

formatGraphInternal :: RDFGraph -> (ShowS, NodeGenLookupMap, Int)
formatGraphInternal gr = 
    let (out, st) = runState (formatGraph gr) emptyNTFS
    in (out, ntfsNodeMap st, ntfsNodeGen st)

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

-- this must be a relatively standard idiom, that can be done
-- better than this
applyShowS :: [String] -> ShowS
applyShowS [] = id
applyShowS (x:xs) = showString x . (applyShowS xs)

formatGraph :: RDFGraph -> Formatter ShowS
formatGraph gr = do
  ls <- mapM formatArc (getArcs gr)
  return $ applyShowS ls

formatArc :: Arc RDFLabel -> Formatter String
formatArc (Arc s p o) = do
  sl <- formatLabel s
  pl <- formatLabel p
  ol <- formatLabel o
  return $ sl ++ " " ++ pl ++ " " ++ ol ++ " .\n"

formatLabel :: RDFLabel -> Formatter String
formatLabel lab@(Blank (lnc:_)) = 
  if isDigit lnc then mapBlankNode lab else return $ show lab
formatLabel lab = return $ show lab

mapBlankNode :: RDFLabel -> Formatter String
mapBlankNode lab = do
  st <- get
  let cmap = ntfsNodeMap st
      cval = ntfsNodeGen st

  nval <- case mapFind 0 lab cmap of
            0 -> do
              let nval = succ cval
                  nmap = mapAdd cmap (lab, nval)

              put $ st { ntfsNodeMap = nmap, ntfsNodeGen = nval }
              return nval

            n -> return n

  return $ show $ Blank ('_':show nval)

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
