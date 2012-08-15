{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Turtle
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--                 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This Module implements a Turtle formatter 
--  for an 'RDFGraph' value.
--
--  REFERENCES:
--
--  - \"Turtle, Terse RDF Triple Language\",
--    W3C Working Draft 09 August 2011 (<http://www.w3.org/TR/2011/WD-turtle-20110809/>)
--    <http://www.w3.org/TR/turtle/>
--
--------------------------------------------------------------------------------

{-
TODO:

The code used to determine whether a blank node can be written
using the "[]" short form could probably take advantage of the
GraphPartition module.

-}

module Swish.RDF.Formatter.Turtle
    ( NodeGenLookupMap
    , formatGraphAsText
    , formatGraphAsLazyText
    , formatGraphAsBuilder
    , formatGraphIndent  
    , formatGraphDiag
    )
where

import Swish.RDF.Formatter.Internal (NodeGenLookupMap, SubjTree, PredTree
                                    , LabelContext(..)
                                    , NodeGenState(..)
                                    , changeState
                                    , hasMore
                                    , emptyNgs
                                    , findMaxBnode
                                    , splitOnLabel
                                    , processArcs
                                    , formatScopedName
                                    , formatPrefixLines
                                    , maybeExtractList
                                    , formatPlainLit
                                    , formatLangLit
                                    , formatTypedLit
                                    , insertList
                                    , nextLine_
                                    , mapBlankNode_
				    )

import Swish.RDF.Graph (
  RDFGraph, RDFLabel(..)
  , NamespaceMap
  , emptyNamespaceMap
  , getNamespaces
  , emptyRDFGraph
  )

import Swish.RDF.Vocabulary (rdfType, rdfNil)

import Control.Monad (liftM)
import Control.Monad.State (State, modify, get, gets, put, runState)

import Data.Char (isDigit)
import Data.Monoid (Monoid(..))
import Data.Word (Word32)

-- it strikes me that using Lazy Text here is likely to be
-- wrong; however I have done no profiling to back this
-- assumption up!

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

----------------------------------------------------------------------
--  Graph formatting state monad
----------------------------------------------------------------------
--
--  The graph to be formatted is carried as part of the formatting
--  state, so that decisions about what needs to be formatted can
--  themselves be based upon and reflected in the state (e.g. if a
--  decision is made to include a blank node inline, it can be removed
--  from the graph state that remains to be formatted).

data TurtleFormatterState = TFS
    { indent    :: B.Builder
    , lineBreak :: Bool
    , graph     :: RDFGraph
    , subjs     :: SubjTree RDFLabel
    , props     :: PredTree RDFLabel   -- for last subject selected
    , objs      :: [RDFLabel]          -- for last property selected
    -- , formAvail :: FormulaMap RDFLabel
    -- , formQueue :: [(RDFLabel,RDFGraph)]
    , prefixes  :: NamespaceMap
    , nodeGenSt :: NodeGenState
    , bNodesCheck   :: [RDFLabel]      -- these bNodes are not to be converted to '[..]' format
    , traceBuf  :: [String]
    }
             
type Formatter a = State TurtleFormatterState a

emptyTFS :: NodeGenState -> TurtleFormatterState
emptyTFS ngs = TFS
    { indent    = "\n"
    , lineBreak = False
    , graph     = emptyRDFGraph
    , subjs     = []
    , props     = []
    , objs      = []
    , prefixes  = emptyNamespaceMap
    , nodeGenSt = ngs
    , bNodesCheck   = []
    , traceBuf  = []
    }

setIndent :: B.Builder -> Formatter ()
setIndent ind = modify $ \st -> st { indent = ind }

setLineBreak :: Bool -> Formatter ()
setLineBreak brk = modify $ \st -> st { lineBreak = brk }

setNgs :: NodeGenState -> Formatter ()
setNgs ngs = modify $ \st -> st { nodeGenSt = ngs }

setSubjs :: SubjTree RDFLabel -> Formatter ()
setSubjs sl = modify $ \st -> st { subjs = sl }

setProps :: PredTree RDFLabel -> Formatter ()
setProps ps = modify $ \st -> st { props = ps }

{-
TODO:

Should we change the preds/objs entries as well?

-}
extractList :: LabelContext -> RDFLabel -> Formatter (Maybe [RDFLabel])
extractList lctxt ln = do
  osubjs <- gets subjs
  oprops <- gets props
  case maybeExtractList osubjs oprops lctxt ln of
    Just (ls, osubjs', oprops') -> do
      setSubjs osubjs'
      setProps oprops'
      return (Just ls)

    _ -> return Nothing
  
----------------------------------------------------------------------
--  Define a top-level formatter function:
----------------------------------------------------------------------

-- | Convert the graph to text.
formatGraphAsText :: RDFGraph -> T.Text
formatGraphAsText = L.toStrict . formatGraphAsLazyText

-- | Convert the graph to text.
formatGraphAsLazyText :: RDFGraph -> L.Text
formatGraphAsLazyText = B.toLazyText . formatGraphAsBuilder
  
-- | Convert the graph to a Builder.
formatGraphAsBuilder :: RDFGraph -> B.Builder
formatGraphAsBuilder = formatGraphIndent "\n" True
  
-- | Convert the graph to a builder using the given indentation text.
formatGraphIndent ::
    B.Builder     -- ^ indentation text
    -> Bool       -- ^ are prefixes to be generated?
    -> RDFGraph   -- ^ graph
    -> B.Builder
formatGraphIndent indnt flag gr = 
  let (res, _, _, _) = formatGraphDiag indnt flag gr
  in res
  
-- | Format graph and return additional information.
formatGraphDiag :: 
  B.Builder  -- ^ indentation
  -> Bool    -- ^ are prefixes to be generated?
  -> RDFGraph 
  -> (B.Builder, NodeGenLookupMap, Word32, [String])
formatGraphDiag indnt flag gr = 
  let fg  = formatGraph indnt " .\n" False flag gr
      ngs = emptyNgs { nodeGen = findMaxBnode gr }
             
      (out, fgs) = runState fg (emptyTFS ngs)
      ogs        = nodeGenSt fgs
  
  in (out, nodeMap ogs, nodeGen ogs, traceBuf fgs)

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

formatGraph :: 
  B.Builder     -- indentation string
  -> B.Builder  -- text to be placed after final statement
  -> Bool       -- True if a line break is to be inserted at the start
  -> Bool       -- True if prefix strings are to be generated
  -> RDFGraph   -- graph to convert
  -> Formatter B.Builder
formatGraph ind end dobreak dopref gr = do
  setIndent ind
  setLineBreak dobreak
  modify (newState gr)
  
  fp <- if dopref
        then formatPrefixes (getNamespaces gr)
        else return mempty
  more <- hasMore subjs
  if more
    then do
      fr <- formatSubjects
      return $ mconcat [fp, fr, end]
    else return fp

formatPrefixes :: NamespaceMap -> Formatter B.Builder
formatPrefixes pmap = 
    mconcat `liftM` mapM nextLine (formatPrefixLines pmap)

{-
NOTE:
I expect there to be confusion below where I need to
convert from Text to Builder
-}

formatSubjects :: Formatter B.Builder
formatSubjects = do
  sb    <- nextSubject
  sbstr <- formatLabel SubjContext sb
  
  flagP <- hasMore props
  if flagP
    then do
      prstr <- formatProperties sb sbstr
      flagS <- hasMore subjs
      if flagS
        then do
          fr <- formatSubjects
          return $ mconcat [prstr, " .", fr]
        else return prstr
           
    else do
      txt <- nextLine sbstr
    
      flagS <- hasMore subjs
      if flagS
        then do
          fr <- formatSubjects
          return $ mconcat [txt, " .", fr]
        else return txt

{-
TODO: now we are throwing a Builder around it is awkward to
get the length of the text to calculate the indentation

So

  a) change the indentation scheme
  b) pass around text instead of builder

mkIndent :: L.Text -> L.Text
mkIndent inVal = L.replicate (L.length inVal) " "
-}

hackIndent :: B.Builder
hackIndent = "    "

formatProperties :: RDFLabel -> B.Builder -> Formatter B.Builder
formatProperties sb sbstr = do
  pr <- nextProperty sb
  prstr <- formatLabel PredContext pr
  obstr <- formatObjects sb pr $ mconcat [sbstr, " ", prstr]
  more  <- hasMore props
  let sbindent = hackIndent -- mkIndent sbstr
  if more
    then do
      fr <- formatProperties sb sbindent
      nl <- nextLine $ obstr `mappend` " ;"
      return $ nl `mappend` fr
    else nextLine obstr

formatObjects :: RDFLabel -> RDFLabel -> B.Builder -> Formatter B.Builder
formatObjects sb pr prstr = do
  ob    <- nextObject sb pr
  obstr <- formatLabel ObjContext ob
  more  <- hasMore objs
  if more
    then do
      let prindent = hackIndent -- mkIndent prstr
      fr <- formatObjects sb pr prindent
      nl <- nextLine $ mconcat [prstr, " ", obstr, ","]
      return $ nl `mappend` fr
    else return $ mconcat [prstr, " ", obstr]

{-
Add a blank node inline.
-}

insertBnode :: LabelContext -> RDFLabel -> Formatter B.Builder
insertBnode SubjContext lbl = do
  -- a safety check
  flag <- hasMore props
  if flag
    then do
      txt <- (`mappend` "\n") `liftM` formatProperties lbl ""
      return $ mconcat ["[] ", txt]
    else error $ "Internal error: expected properties with label: " ++ show lbl

insertBnode _ lbl = do
  ost <- get
  let osubjs = subjs ost
      (rsubjs, rprops) = splitOnLabel lbl osubjs
      nst = ost { subjs = rsubjs,
                  props = rprops,
                  objs  = []
                }

  put nst
  flag <- hasMore props
  txt <- if flag
         then (`mappend` "\n") `liftM` formatProperties lbl ""
         else return ""

  -- restore the original data (where appropriate)
  nst' <- get
  let slist  = map fst $ subjs nst'
      nsubjs = filter (\(l,_) -> l `elem` slist) osubjs

  put $ nst' { subjs = nsubjs,
                       props = props ost, 
                       objs  = objs ost
             }

  -- TODO: handle indentation?
  return $ mconcat ["[", txt, "]"]
  
----------------------------------------------------------------------
--  Formatting helpers
----------------------------------------------------------------------

newState :: RDFGraph -> TurtleFormatterState -> TurtleFormatterState
newState gr st = 
    let pre' = prefixes st `M.union` getNamespaces gr
        (arcSubjs, bNodes) = processArcs gr

    in st  { graph     = gr
           , subjs     = arcSubjs
           , props     = []
           , objs      = []
           , prefixes  = pre'
           , bNodesCheck   = bNodes
           }

nextSubject :: Formatter RDFLabel
nextSubject = 
    changeState $ \st -> 
        let (a,b):sbs = subjs st
            nst = st  { subjs = sbs
                      , props = b
                      , objs  = []
                      }
        in (a, nst)

nextProperty :: RDFLabel -> Formatter RDFLabel
nextProperty _ =
    changeState $ \st ->
        let (a,b):prs = props st
            nst = st  { props = prs
                      , objs  = b
                      }
        in (a, nst)
        
nextObject :: RDFLabel -> RDFLabel -> Formatter RDFLabel
nextObject _ _ =
    changeState $ \st ->
        let ob:obs = objs st
            nst = st { objs = obs }
        in (ob, nst)

nextLine :: B.Builder -> Formatter B.Builder
nextLine = nextLine_ indent lineBreak setLineBreak

--  Format a label
--  Most labels are simply displayed as provided, but there are a
--  number of wrinkles to take care of here:
--  (a) blank nodes automatically allocated on input, with node
--      identifiers of the form of a digit string nnn.  These are
--      not syntactically valid, and are reassigned node identifiers
--      of the form _nnn, where nnn is chosen so that is does not
--      clash with any other identifier in the graph.
--  (b) URI nodes:  if possible, replace URI with qname,
--      else display as <uri>
--  (c) formula nodes (containing graphs).
--  (d) use the "special-case" formats for integer/float/double
--      literals.      
--      
--  [[[TODO:]]]
--  (d) generate multi-line literals when appropriate
--
-- This is being updated to produce inline formula, lists and     
-- blank nodes. The code is not efficient.
--
--
-- Note: There is a lot less customisation possible in Turtle than N3.
--      

formatLabel :: LabelContext -> RDFLabel -> Formatter B.Builder

{-
The "[..]" conversion is done last, after "()" and "{}" checks.

TODO: look at the (_:_) check on the blank string; why is this needed?
-}
formatLabel lctxt lab@(Blank (_:_)) = do
  mlst <- extractList lctxt lab
  case mlst of
    Just lst -> insertList (formatLabel ObjContext) lst
    Nothing -> do
      -- NOTE: unlike N3 we do not properly handle "formula"/named graphs
      -- also we only expand out bnodes into [...] format when it's a object.
      -- although we need to handle [] for the subject.
      nb1 <- gets bNodesCheck
      if lctxt /= PredContext && lab `notElem` nb1
        then insertBnode lctxt lab
        else formatNodeId lab

-- formatLabel _ lab@(Res sn) = 
formatLabel ctxt (Res sn)
  | ctxt == PredContext && sn == rdfType = return "a"
  | ctxt == ObjContext  && sn == rdfNil  = return "()"
  | otherwise = do
  pr <- gets prefixes
  return $ formatScopedName sn pr

formatLabel _ (Lit lit) = return $ formatPlainLit lit
formatLabel _ (LangLit lit lcode) = return $ formatLangLit lit lcode
formatLabel _ (TypedLit lit dtype) = return $ formatTypedLit lit dtype

formatLabel _ lab = return $ B.fromString $ show lab

formatNodeId :: RDFLabel -> Formatter B.Builder
formatNodeId lab@(Blank (lnc:_)) =
    if isDigit lnc then mapBlankNode lab else return $ B.fromString $ show lab
formatNodeId other = error $ "formatNodeId not expecting a " ++ show other -- to shut up -Wall

mapBlankNode :: RDFLabel -> Formatter B.Builder
mapBlankNode = mapBlankNode_ nodeGenSt setNgs

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
