{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3Formatter
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This Module implements a NTriples formatter (see [1])
--  for an RDFGraph value.
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
    , formatGraphAsText
    , formatGraphAsLazyText
    , formatGraphAsBuilder
    )
where

import Swish.RDF.RDFGraph
  ( RDFGraph, RDFLabel(..)
  , getArcs
  )

import Swish.RDF.GraphClass
    ( Arc(..) )

import Swish.Utils.Namespace (ScopedName, getQName)
import Swish.RDF.Vocabulary (langTag)

import Swish.Utils.LookupMap
    ( LookupMap, emptyLookupMap
    , mapFind, mapAdd
    )

import Data.Char (ord, intToDigit, toUpper)

import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Monoid

-- it strikes me that using Lazy Text here is likely to be
-- wrong; however I have done no profiling to back this
-- assumption up!

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

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

-- | Convert a RDF graph to NTriples format.
formatGraphAsText :: RDFGraph -> T.Text
formatGraphAsText = L.toStrict . formatGraphAsLazyText

-- | Convert a RDF graph to NTriples format.
formatGraphAsLazyText :: RDFGraph -> L.Text
formatGraphAsLazyText = B.toLazyText . formatGraphAsBuilder

-- | Convert a RDF graph to NTriples format.
formatGraphAsBuilder :: RDFGraph -> B.Builder
formatGraphAsBuilder gr = fst $ runState (formatGraph gr) emptyNTFS

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

formatGraph :: RDFGraph -> Formatter B.Builder
formatGraph gr = mconcat <$> mapM formatArc (getArcs gr)

-- TODO: this reverses the contents but may be faster?
--       that is if I've got the order right in the mappend call
-- formatGraphBuilder gr = foldl' (\a b -> b `mappend` (formatArcBuilder a)) B.empty (getArcs gr)

space, nl :: B.Builder
space = B.singleton ' '
nl    = " .\n"

formatArc :: Arc RDFLabel -> Formatter B.Builder
formatArc (Arc s p o) = do
  sl <- formatLabel s
  pl <- formatLabel p
  ol <- formatLabel o
  return $ mconcat [sl, space, pl, space, ol, nl]
  -- return $ sl `mappend` $ space `mappend` $ pl `mappend` $ space `mappend` $ ol `mappend` nl
  
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

squote, at, carets  :: B.Builder
squote = "\""
at     = "@"
carets = "^^"

formatLabel :: RDFLabel -> Formatter B.Builder
formatLabel lab@(Blank _) = mapBlankNode lab
formatLabel (Res sn) = return $ showScopedName sn
formatLabel (Lit lit) = return $ quoteText lit
formatLabel (LangLit lit lang) = return $ mconcat [quoteText lit, at, B.fromText (langTag lang)]
formatLabel (TypedLit lit dt)  = return $ mconcat [quoteText lit, carets, showScopedName dt]

-- do not expect to get the following, but include
-- just in case rather than failing
formatLabel lab = return $ B.fromString $ show lab

mapBlankNode :: RDFLabel -> Formatter B.Builder
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

  return $ "_:swish" `mappend` B.fromString (show nv)

-- TODO: can we use Network.URI to protect the URI?
showScopedName :: ScopedName -> B.Builder
{-
showScopedName (ScopedName n l) = 
  let uri = T.pack (show (nsURI n)) `mappend` l
  in mconcat ["<", B.fromText (quote uri), ">"]
-}
-- showScopedName s = mconcat ["<", B.fromText (quote (T.pack (show (getQName s)))), ">"]
showScopedName s = B.fromText (quote (T.pack (show (getQName s)))) -- looks like qname already adds the <> around this

quoteText :: T.Text -> B.Builder
quoteText  st = mconcat [squote, B.fromText (quote st), squote]

{-
QUS: should we be operating on Text like this?
-}

quote :: T.Text -> T.Text
quote = T.concatMap quoteT

quoteT :: Char -> T.Text
quoteT '\\' = "\\\\"
quoteT '"'  = "\\\""
quoteT '\n' = "\\n"
quoteT '\t' = "\\t"
quoteT '\r' = "\\r"
quoteT c    = 
  let nc = ord c
      
  in if nc > 0xffff 
     then T.pack ('\\':'U': numToHex 8 nc)
     else if nc > 0x7e || nc < 0x20
          then T.pack ('\\':'u': numToHex 4 nc)
          else T.singleton c
                      
-- we assume c > 0, n >= 0 and that the input value fits
-- into the requested number of digits
numToHex :: Int -> Int -> String
numToHex c = go []
  where
    go s 0 = replicate (c - length s) '0' ++ s
    go s n = 
      let (m,x) = divMod n 16
      in go (iToD x:s) m

    -- Data.Char.intToDigit uses lower-case Hex
    iToD x | x < 10    = intToDigit x
           | otherwise = toUpper $ intToDigit x
      
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
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
