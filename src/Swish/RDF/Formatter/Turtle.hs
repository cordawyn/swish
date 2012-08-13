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
      
    --  -- * Auxillary routines
    -- , quoteText
    )
where

import Swish.RDF.Formatter.Internal (NodeGenLookupMap, SubjTree, PredTree
                                    , LabelContext(..)
                                    , NodeGenState(..), emptyNgs
                                    , findMaxBnode
                                    , getCollection
                                    , processArcs
				    , findPrefix
				    , quoteB
				    , showScopedName
				    )

import Swish.Namespace (getScopeLocal, getScopeURI)
import Swish.QName (getLName)

import Swish.RDF.Graph (
  RDFGraph, RDFLabel(..)
  , NamespaceMap
  , getNamespaces
  , emptyRDFGraph
  , quoteT
  , resRdfFirst, resRdfRest
  )

import Swish.RDF.Vocabulary ( fromLangTag 
                            , rdfType
                            , rdfNil
                            , xsdBoolean, xsdDecimal, xsdInteger, xsdDouble 
                            )

import Control.Monad (liftM, when)
import Control.Monad.State (State, modify, get, put, runState)

import Data.Char (isDigit)
import Data.List (partition, intersperse)
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
    -- , formAvail = emptyFormulaMap
    -- , formQueue = []
    , nodeGenSt = ngs
    , bNodesCheck   = []
    , traceBuf  = []
    }

getIndent :: Formatter B.Builder
getIndent = indent `liftM` get

setIndent :: B.Builder -> Formatter ()
setIndent ind = modify $ \st -> st { indent = ind }

getLineBreak :: Formatter Bool
getLineBreak = lineBreak `liftM` get

setLineBreak :: Bool -> Formatter ()
setLineBreak brk = modify $ \st -> st { lineBreak = brk }

getNgs :: Formatter NodeGenState
getNgs = nodeGenSt `liftM` get

setNgs :: NodeGenState -> Formatter ()
setNgs ngs = modify $ \st -> st { nodeGenSt = ngs }

getPrefixes :: Formatter NamespaceMap
getPrefixes = prefixes `liftM` getNgs

getSubjs :: Formatter (SubjTree RDFLabel)
getSubjs = subjs `liftM` get

setSubjs :: SubjTree RDFLabel -> Formatter ()
setSubjs sl = modify $ \st -> st { subjs = sl }

getProps :: Formatter (PredTree RDFLabel)
getProps = props `liftM` get

setProps :: PredTree RDFLabel -> Formatter ()
setProps ps = modify $ \st -> st { props = ps }

{-
getObjs :: Formatter ([RDFLabel])
getObjs = objs `liftM` get

setObjs :: [RDFLabel] -> Formatter ()
setObjs os = do
  st <- get
  put $ st { objs = os }
-}

getBnodesCheck :: Formatter [RDFLabel]
getBnodesCheck = bNodesCheck `liftM` get

{-
addTrace :: String -> Formatter ()
addTrace tr = do
  st <- get
  put $ st { traceBuf = tr : traceBuf st }
-}
  
{-
TODO:

Should we change the preds/objs entries as well?

-}
extractList :: LabelContext -> RDFLabel -> Formatter (Maybe [RDFLabel])
extractList lctxt ln = do
  osubjs <- getSubjs
  oprops <- getProps
  let mlst = getCollection osubjs' ln

      -- we only want to send in rdf:first/rdf:rest here
      fprops = filter ((`elem` [resRdfFirst, resRdfRest]) . fst) oprops

      osubjs' =
          case lctxt of
            SubjContext -> (ln, fprops) : osubjs
            _ -> osubjs 

      -- tr = "extractList " ++ show ln ++ " (" ++ show lctxt ++ ")\n -> osubjs= " ++ show osubjs ++ "\n -> opreds= " ++ show oprops ++ "\n -> mlst= " ++ show mlst ++ "\n"
  -- addTrace tr
  case mlst of
    -- sl is guaranteed to be free of (ln,fprops) here if lctxt is SubjContext
    Just (sl,ls,_) -> do
              setSubjs sl
              when (lctxt == SubjContext) $ setProps $ filter ((`notElem` [resRdfFirst, resRdfRest]) . fst) oprops
              return (Just ls)

    Nothing -> return Nothing
  
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
      ngs = emptyNgs {
        prefixes = M.empty,
        nodeGen  = findMaxBnode gr
        }
             
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
  setGraph gr
  
  fp <- if dopref
        then formatPrefixes (getNamespaces gr)
        else return mempty
  more <- moreSubjects
  if more
    then do
      fr <- formatSubjects
      return $ mconcat [fp, fr, end]
    else return fp

formatPrefixes :: NamespaceMap -> Formatter B.Builder
formatPrefixes pmap = do
  let mls = map pref $ M.assocs pmap
  ls <- sequence mls
  return $ mconcat ls
    where
      pref (Just p,u) = nextLine $ mconcat ["@prefix ", B.fromText p, ": <", quoteB True (show u), "> ."]
      pref (_,u)      = nextLine $ mconcat ["@prefix : <", quoteB True (show u), "> ."]

{-
NOTE:
I expect there to be confusion below where I need to
convert from Text to Builder
-}

formatSubjects :: Formatter B.Builder
formatSubjects = do
  sb    <- nextSubject
  sbstr <- formatLabel SubjContext sb
  
  flagP <- moreProperties
  if flagP
    then do
      prstr <- formatProperties sb sbstr
      flagS <- moreSubjects
      if flagS
        then do
          fr <- formatSubjects
          return $ mconcat [prstr, " .", fr]
        else return prstr
           
    else do
      txt <- nextLine sbstr
    
      flagS <- moreSubjects
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
  more  <- moreProperties
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
  more  <- moreObjects
  if more
    then do
      let prindent = hackIndent -- mkIndent prstr
      fr <- formatObjects sb pr prindent
      nl <- nextLine $ mconcat [prstr, " ", obstr, ","]
      return $ nl `mappend` fr
    else return $ mconcat [prstr, " ", obstr]

{-
Add a list inline. We are given the labels that constitute
the list, in order, so just need to display them surrounded
by ().
-}
insertList :: [RDFLabel] -> Formatter B.Builder
insertList [] = return "()" -- not convinced this can happen
insertList xs = do
  ls <- mapM (formatLabel ObjContext) xs
  return $ mconcat ("( " : intersperse " " ls) `mappend` " )"
    
{-
Add a blank node inline.
-}

insertBnode :: LabelContext -> RDFLabel -> Formatter B.Builder
insertBnode SubjContext lbl = do
  -- a safety check
  flag <- moreProperties
  if flag
    then do
      txt <- (`mappend` "\n") `liftM` formatProperties lbl ""
      return $ mconcat ["[] ", txt]
    else error $ "Internal error: expected properties with label: " ++ show lbl

insertBnode _ lbl = do
  ost <- get
  let osubjs = subjs ost
      oprops = props ost
      oobjs  = objs  ost

      (bsubj, rsubjs) = partition ((== lbl) . fst) osubjs

      rprops = case bsubj of
                 [(_,rs)] -> rs
                 _ -> []

      -- we essentially want to create a new subgraph
      -- for this node but it's not as simple as that since
      -- we could have something like
      --     :a :b [ :foo [ :bar "xx" ] ]
      -- so we still need to carry around the whole graph
      --
      nst = ost { subjs = rsubjs,
                  props = rprops,
                  objs  = []
                }

  put nst
  flag <- moreProperties
  txt <- if flag
         then (`mappend` "\n") `liftM` formatProperties lbl ""
         else return ""

  -- TODO: how do we restore the original set up?
  --       I can't believe the following is sufficient
  --
  nst' <- get
  let slist  = map fst $ subjs nst'
      nsubjs = filter (\(l,_) -> l `elem` slist) osubjs

  put $ nst' { subjs = nsubjs,
                       props = oprops, 
                       objs  = oobjs
             }

  -- TODO: handle indentation?
  return $ mconcat ["[", txt, "]"]
  
----------------------------------------------------------------------
--  Formatting helpers
----------------------------------------------------------------------

newState :: RDFGraph -> TurtleFormatterState -> TurtleFormatterState
newState gr st = 
    let ngs0 = nodeGenSt st
        pre' = prefixes ngs0 `M.union` getNamespaces gr
        ngs' = ngs0 { prefixes = pre' }
        (arcSubjs, bNodes) = processArcs gr

    in st  { graph     = gr
           , subjs     = arcSubjs
           , props     = []
           , objs      = []
           , nodeGenSt = ngs'
           , bNodesCheck   = bNodes
           }

setGraph :: RDFGraph -> Formatter ()
setGraph = modify . newState

hasMore :: (TurtleFormatterState -> [b]) -> Formatter Bool
hasMore lens = (not . null . lens) `liftM` get

moreSubjects :: Formatter Bool
moreSubjects = hasMore subjs

moreProperties :: Formatter Bool
moreProperties = hasMore props

moreObjects :: Formatter Bool
moreObjects = hasMore objs

nextSubject :: Formatter RDFLabel
nextSubject = do
  st <- get

  let sb:sbs = subjs st
      nst = st  { subjs = sbs
                , props = snd sb
                , objs  = []
                }

  put nst
  return $ fst sb

nextProperty :: RDFLabel -> Formatter RDFLabel
nextProperty _ = do
  st <- get

  let pr:prs = props st
      nst = st  { props = prs
                 , objs  = snd pr
                 }

  put nst
  return $ fst pr

nextObject :: RDFLabel -> RDFLabel -> Formatter RDFLabel
nextObject _ _ = do
  st <- get

  let ob:obs = objs st
      nst = st { objs = obs }

  put nst
  return ob

nextLine :: B.Builder -> Formatter B.Builder
nextLine str = do
  ind <- getIndent
  brk <- getLineBreak
  if brk
    then return $ ind `mappend` str
    else do
      --  After first line, always insert line break
      setLineBreak True
      return str

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
-}
formatLabel lctxt lab@(Blank (_:_)) = do
  mlst <- extractList lctxt lab
  case mlst of
    Just lst -> insertList lst
    Nothing -> do
      -- NOTE: unlike N3 we do not properly handle "formula"/named graphs
      -- also we only expand out bnodes into [...] format when it's a object.
      -- although we need to handle [] for the subject.
      nb1 <- getBnodesCheck
      if lctxt /= PredContext && lab `notElem` nb1
        then insertBnode lctxt lab
        else formatNodeId lab

-- formatLabel _ lab@(Res sn) = 
formatLabel ctxt (Res sn)
  | ctxt == PredContext && sn == rdfType = return "a"
  | ctxt == ObjContext  && sn == rdfNil  = return "()"
  | otherwise = do
  pr <- getPrefixes
  let nsuri  = getScopeURI sn
      local  = getLName $ getScopeLocal sn
      name   = case findPrefix nsuri pr of
        Just (Just p) -> B.fromText $ quoteT True $ mconcat [p, ":", local] -- TODO: what are quoting rules for QNames
        _ -> mconcat ["<", quoteB True (show nsuri ++ T.unpack local), ">"]
      
  return name

-- The canonical notation for xsd:double in XSD, with an upper-case E,
-- does not match the syntax used in N3, so we need to convert here.     
-- Rather than converting back to a Double and then displaying that       
-- we just convert E to e for now.      
--      
formatLabel _ (Lit lit) = return $ quoteText lit
formatLabel _ (LangLit lit lcode) = return $ quoteText lit `mappend` "@" `mappend` B.fromText (fromLangTag lcode)
formatLabel _ (TypedLit lit dtype)
    | dtype == xsdDouble = return $ B.fromText $ T.toLower lit
    | dtype `elem` [xsdBoolean, xsdDecimal, xsdInteger] = return $ B.fromText lit
    | otherwise = return $ quoteText lit `mappend` "^^" `mappend` showScopedName dtype

formatLabel _ lab = return $ B.fromString $ show lab

{-|
Convert text into a format for display in Turtle. The idea
is to use one double quote unless three are needed, and to
handle adding necessary @\\@ characters, or conversion
for Unicode characters.
-}
quoteText :: T.Text -> B.Builder
quoteText txt = 
  let st = T.unpack txt -- TODO: fix
      qst = quoteB (n==1) st
      n = if '\n' `elem` st || '"' `elem` st then 3 else 1
      qch = B.fromString (replicate n '"')
  in mconcat [qch, qst, qch]

formatNodeId :: RDFLabel -> Formatter B.Builder
formatNodeId lab@(Blank (lnc:_)) =
    if isDigit lnc then mapBlankNode lab else return $ B.fromString $ show lab
formatNodeId other = error $ "formatNodeId not expecting a " ++ show other -- to shut up -Wall

mapBlankNode :: RDFLabel -> Formatter B.Builder
mapBlankNode lab = do
  ngs <- getNgs
  let cmap = nodeMap ngs
      cval = nodeGen ngs
  nv <- case M.findWithDefault 0 lab cmap of
    0 -> do 
      let nval = succ cval
          nmap = M.insert lab nval cmap
      setNgs $ ngs { nodeGen = nval, nodeMap = nmap }
      return nval
      
    n -> return n
  
  -- TODO: is this what we want?
  return $ "_:swish" `mappend` B.fromString (show nv)

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
