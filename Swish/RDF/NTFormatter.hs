--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3Formatter
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
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

module Swish.RDF.NTFormatter
    ( NodeGenLookupMap
    , formatGraphAsString
    , formatGraphAsShowS
    )
where

import Swish.RDF.RDFGraph
  ( RDFGraph, RDFLabel(..)
  , getArcs
  )

import Swish.RDF.GraphClass
    ( Arc(..) )

import Swish.Utils.Namespace (ScopedName(..), nsURI)
import Swish.RDF.Vocabulary (isLang, langTag)

import Swish.Utils.LookupMap
    ( LookupMap, emptyLookupMap
    , mapFind, mapAdd
    )

import Text.Printf (printf)
import Data.Char (ord)

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

-- Are there better ways to do this (could look at moving to a Builder
-- style system)?
-- 
applyShowS :: [ShowS] -> ShowS
applyShowS = foldr (.) id

formatGraph :: RDFGraph -> Formatter ShowS
formatGraph gr = do
  ls <- mapM formatArc (getArcs gr)
  return $ applyShowS ls

formatArc :: Arc RDFLabel -> Formatter ShowS
formatArc (Arc s p o) = do
  sl <- formatLabel s
  pl <- formatLabel p
  ol <- formatLabel o
  return $ applyShowS $ map showString [sl, " ", pl, " ", ol, " .\n"]

{-
If we have a blank node then can

  - use the label it contains
  - generate a new one on output

For now we create new labels whatever the input was since this
simplifies things, but it may be changed.

formatLabel :: RDFLabel -> Formatter String
formatLabel lab@(Blank (lnc:_)) = 
  if isDigit lnc then mapBlankNode lab else return $ show lab
formatLabel lab = return $ show lab
-}

formatLabel :: RDFLabel -> Formatter String
formatLabel lab@(Blank _) = mapBlankNode lab
formatLabel (Res sn) = return $ showScopedName sn
formatLabel (Lit lit Nothing) = return $ quoteStr lit
formatLabel (Lit lit (Just nam)) | isLang nam = return $ quoteStr lit ++ "@" ++ langTag nam
                                 | otherwise  = return $ quoteStr lit ++ "^^" ++ showScopedName nam

-- do not expect to get the following, but include
-- just in case rather than failing
formatLabel lab = return $ show lab

mapBlankNode :: RDFLabel -> Formatter String
mapBlankNode lab = do
  st <- get
  let cmap = ntfsNodeMap st
      cval = ntfsNodeGen st

  nv <- case mapFind 0 lab cmap of
            0 -> do
              let nval = succ cval
                  nmap = mapAdd cmap (lab, nval)

              put $ st { ntfsNodeMap = nmap, ntfsNodeGen = nval }
              return nval

            n -> return n

  return $ "_:swish" ++ show nv

showScopedName :: ScopedName -> String
showScopedName (ScopedName n l) = 
  let uri = nsURI n ++ l
  in "<" ++ quote uri ++ ">"

{-
Swish.Utils.MiscHelpers contains a quote routine
which we expand upon here to match the NT syntax.
-}

quoteStr :: String -> String
quoteStr  st = ['"'] ++ quote st ++ ['"']

quote :: String -> String
quote []           = ""
quote ('\\':st)    = '\\':'\\': quote st
quote ('"': st)    = '\\':'"': quote st
quote ('\n':st)    = '\\':'n': quote st
quote ('\r':st)    = '\\':'r': quote st
quote ('\t':st)    = '\\':'t': quote st
quote (c:st) = 
  let nc = ord c
      rst = quote st
      
      -- lazy way to convert to a string
      hstr = printf "%08X" nc
      ustr = hstr ++ rst

  in if nc > 0xffff 
     then '\\':'U': ustr
     else if nc > 0x7e || nc < 0x20
          then '\\':'u': drop 4 ustr
          else c : rst
                      
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
