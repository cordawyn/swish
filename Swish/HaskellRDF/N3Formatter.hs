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
--  This Module implements a Notation 3 formatter (see [1], [2] and [3]),
--  for an RDFGraph value.
--
--
-- REFERENCES:
--
-- (1) <http://www.w3.org/TeamSubmission/2008/SUBM-n3-20080114/>
--     Notation3 (N3): A readable RDF syntax,
--     W3C Team Submission 14 January 2008
--
-- (2) <http://www.w3.org/DesignIssues/Notation3.html>
--     Tim Berners-Lee's design issues series notes and description
--
-- (2) <http://www.w3.org/2000/10/swap/Primer.html>
--     Notation 3 Primer by Sean Palmer
--
--
--  TODO:
--
--   * Initial prefix list to include nested formulae;
--      then don't need to update prefix list for these.
--
--   * blank nodes used just once, can be expanded inline using
--     [...] syntax.
--
--   * generate multi-line literals when appropriate
--
--   * more flexible terminator generation for formatted formulae
--     (for inline blank nodes.)
--
--------------------------------------------------------------------------------

{-
TODO:

*) base issues

do not have test failures for these

-}

module Swish.HaskellRDF.N3Formatter
    ( NodeGenLookupMap
    , formatGraphAsStringNl
    , formatGraphAsString
    , formatGraphAsShowS
    , formatGraphIndent
    , formatGraphDiag
    )
where

import Swish.HaskellRDF.RDFGraph (
  RDFGraph, RDFLabel(..),
  NamespaceMap, RevNamespaceMap,
  emptyNamespaceMap,
  FormulaMap, emptyFormulaMap,
  getArcs, labels,
  setNamespaces, getNamespaces,
  getFormulae,
  emptyRDFGraph,
  res_rdf_first, res_rdf_rest, res_rdf_nil
  )

import Swish.HaskellRDF.Vocabulary (
  isLang, langTag, 
  rdf_type,
  rdf_nil,
  owl_sameAs, log_implies
  )

import Swish.HaskellRDF.GraphClass
    ( Arc(..) )

import Swish.HaskellUtils.LookupMap
    ( LookupEntryClass(..)
    , LookupMap, emptyLookupMap, reverseLookupMap
    , listLookupMap
    , mapFind, mapFindMaybe, mapAdd, mapDelete, mapMerge
    )

import Swish.HaskellUtils.Namespace
    ( ScopedName(..), getScopeURI )

import Swish.HaskellRDF.Sort.QuickSort
    ( stableQuickSort )

import Data.Char (ord, isDigit)

import Data.List (foldl', delete, groupBy, intercalate, partition)

import Text.Printf (printf)

import Control.Monad (liftM, when)
import Control.Monad.State (State, get, put, runState)

----------------------------------------------------------------------
--  Ouptut string concatenation
----------------------------------------------------------------------
--
--  Function puts uses the shows mechanism to avoid the cost of
--  quadratic string concatenation times.  (Use function composition to
--  concatenate strings thus reprersented.)

puts :: String -> ShowS
puts = showString

----------------------------------------------------------------------
--  Graph formatting state monad
----------------------------------------------------------------------
--
--  The graph to be formatted is carried as part of the formatting
--  state, so that decisions about what needs to be formatted can
--  themselves be based upon and reflected in the state (e.g. if a
--  decision is made to include a blank node inline, it can be removed
--  from the graph state that remains to be formatted).

type SubjTree lb = [(lb,PredTree lb)]
type PredTree lb = [(lb,[lb])]

data N3FormatterState = N3FS
    { indent    :: String
    , lineBreak :: Bool
    , graph     :: RDFGraph
    , subjs     :: SubjTree RDFLabel
    , props     :: PredTree RDFLabel   -- for last subject selected
    , objs      :: [RDFLabel]          -- for last property selected
    , formAvail :: FormulaMap RDFLabel
    , formQueue :: [(RDFLabel,RDFGraph)]
    , nodeGenSt :: NodeGenState
    , bNodesCheck   :: [RDFLabel]      -- these bNodes are not to be converted to '[..]' format
    , traceBuf  :: [String]
    }

type Formatter a = State N3FormatterState a

emptyN3FS :: NodeGenState -> N3FormatterState
emptyN3FS ngs = N3FS
    { indent    = "\n"
    , lineBreak = False
    , graph     = emptyRDFGraph
    , subjs     = []
    , props     = []
    , objs      = []
    , formAvail = emptyFormulaMap
    , formQueue = []
    , nodeGenSt = ngs
    , bNodesCheck   = []
    , traceBuf  = []
    }

--  | Node name generation state information that carries through
--  and is updated by nested formulae
type NodeGenLookupMap = LookupMap (RDFLabel,Int)

data NodeGenState = Ngs
    { prefixes  :: NamespaceMap
    , nodeMap   :: NodeGenLookupMap
    , nodeGen   :: Int
    }

emptyNgs :: NodeGenState
emptyNgs = Ngs
    { prefixes  = emptyLookupMap
    , nodeMap   = emptyLookupMap
    , nodeGen   = 0
    }

-- simple context for label creation
-- (may be a temporary solution to the problem
--  of label creation)
--
data LabelContext = SubjContext | PredContext | ObjContext
                    deriving (Eq, Show)

getIndent :: Formatter String
getIndent = indent `liftM` get

setIndent :: String -> Formatter ()
setIndent ind = do
  st <- get
  put $ st { indent = ind }

getLineBreak :: Formatter Bool
getLineBreak = lineBreak `liftM` get

setLineBreak :: Bool -> Formatter ()
setLineBreak brk = do
  st <- get
  put $ st {lineBreak = brk}

getNgs :: Formatter NodeGenState
getNgs = nodeGenSt `liftM` get

setNgs :: NodeGenState -> Formatter ()
setNgs ngs = do
  st <- get
  put $ st { nodeGenSt = ngs }

getPrefixes :: Formatter NamespaceMap
getPrefixes = prefixes `liftM` getNgs

getSubjs :: Formatter (SubjTree RDFLabel)
getSubjs = subjs `liftM` get

setSubjs :: SubjTree RDFLabel -> Formatter ()
setSubjs sl = do
  st <- get
  put $ st { subjs = sl }

getProps :: Formatter (PredTree RDFLabel)
getProps = props `liftM` get

setProps :: PredTree RDFLabel -> Formatter ()
setProps ps = do
  st <- get
  put $ st { props = ps }

{-
getObjs :: Formatter ([RDFLabel])
getObjs = objs `liftM` get

setObjs :: [RDFLabel] -> Formatter ()
setObjs os = do
  st <- get
  put $ st { objs = os }
-}

getBnodesCheck :: Formatter ([RDFLabel])
getBnodesCheck = bNodesCheck `liftM` get

{-
addTrace :: String -> Formatter ()
addTrace tr = do
  st <- get
  put $ st { traceBuf = tr : traceBuf st }
-}
  
queueFormula :: RDFLabel -> Formatter ()
queueFormula fn = do
  st <- get
  let fa = formAvail st
      newState fv = st {
                      formAvail = mapDelete fa fn,
                      formQueue = (fn,fv) : formQueue st
                    }
  case mapFindMaybe fn fa of
    Nothing -> return ()
    Just v -> put (newState v) >> return ()

{-
Return the graph associated with the label and delete it
from the store, if there is an association, otherwise
return Nothing.
-}
extractFormula :: RDFLabel -> Formatter (Maybe RDFGraph)
extractFormula fn = do
  st <- get
  let fa = formAvail st
      newState = st { formAvail=mapDelete fa fn }
  case mapFindMaybe fn fa of
    Nothing -> return Nothing
    Just fv -> put newState >> return (Just fv)

moreFormulae :: Formatter Bool
moreFormulae =  do
  st <- get
  return $ not $ null (formQueue st)

nextFormula :: Formatter (RDFLabel,RDFGraph)
nextFormula = do
  st <- get
  let (nf : fq) = formQueue st
  put $ st { formQueue = fq }
  return nf

-- list has a length of 1
len1 :: [a] -> Bool
len1 (_:[]) = True
len1 _ = False

{-|
Given a set of statements and a label, return the details of the
RDF collection referred to by label, or Nothing.

For label to be considered as representing a collection we require the
following conditions to hold (this is only to support the
serialisation using the '(..)' syntax and does not make any statement
about semantics of the statements with regard to RDF Collections):

  - there must be one rdf_first and one rdf_rest statement
  - there must be no other predicates for the label

-} 
getCollection ::          
  SubjTree RDFLabel -- ^ statements organized by subject
  -> RDFLabel -- ^ does this label represent a list?
  -> Maybe (SubjTree RDFLabel, [RDFLabel], [RDFLabel])
     -- ^ the statements with the elements removed; the
     -- content elements of the collection (the objects of the rdf:first
     -- predicate) and the nodes that represent the spine of the
     -- collection (in reverse order, unlike the actual contents which are in
     -- order).
getCollection subjList lbl = go subjList lbl ([],[]) 
    where
      go sl l (cs,ss) | l == res_rdf_nil = Just (sl, reverse cs, ss)
                      | otherwise = do
        (pList1, sl') <- removeItem sl l
        (pFirst, pList2) <- removeItem pList1 res_rdf_first
        (pNext, pList3) <- removeItem pList2 res_rdf_rest

        -- QUS: could I include these checks implicitly in the pattern matches above?
        -- ie instrad of (pFirst, pos1) <- ..
        -- have ([content], pos1) <- ...
        -- ?
        if and [len1 pFirst, len1 pNext, null pList3]
          then go sl' (head pNext) (head pFirst : cs, l : ss)
          else Nothing

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
      fprops = filter ((`elem` [res_rdf_first, res_rdf_rest]) . fst) oprops

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
              when (lctxt == SubjContext) $ setProps $ filter ((`notElem` [res_rdf_first, res_rdf_rest]) . fst) oprops
              return (Just ls)

    Nothing -> return Nothing
  
{-
-- for safety I am assuming no ordering of the subject tree
-- but really should be using one of the container types
--    
deleteItems :: (Eq a) => [(a,b)] -> [a] -> [(a,b)]
deleteItems [] _  = []
deleteItems os [] = os
deleteItems os (x:xs) =
  deleteItems (deleteItem os x) xs
    
deleteItem :: (Eq a) => [(a,b)] -> a -> [(a,b)]
deleteItem os x =
  case removeItem os x of
    Just (_, rest) -> rest
    Nothing -> os
-}

{-|
Removes the first occurrence of the item from the
association list, returning it's contents and the rest
of the list, if it exists.
-}
removeItem :: (Eq a) => [(a,b)] -> a -> Maybe (b, [(a,b)])
removeItem os x =
  let (as, bs) = break (\a -> fst a == x) os
  in case bs of
    ((_,b):bbs) -> Just (b, as ++ bbs)
    [] -> Nothing

----------------------------------------------------------------------
--  Define a top-level formatter function:
--  accepts a graph and returns a string
----------------------------------------------------------------------

formatGraphAsStringNl :: RDFGraph -> String
formatGraphAsStringNl gr = formatGraphAsShowS gr "\n"

formatGraphAsString :: RDFGraph -> String
formatGraphAsString gr = formatGraphAsShowS gr ""

formatGraphAsShowS :: RDFGraph -> ShowS
formatGraphAsShowS = formatGraphIndent "\n" True
{- old code:
    where
        (out,_,_,_) = formatGraphDiag gr
-}

formatGraphIndent :: String -> Bool -> RDFGraph -> ShowS
{- working version
formatGraphIndent ind dopref gr = out
    where
        (_,out) = formatGraphDiag1 ind dopref emptyLookupMap gr
-}
formatGraphIndent ind dopref = fst . formatGraphDiag1 ind dopref emptyLookupMap
{-      
formatGraphIndent ind dopref gr = out
    where
        (out',fgs) = formatGraphDiag1 ind dopref emptyLookupMap gr
        tbuff = traceBuf fgs
        -- tr = if null tbuff then "" else "\nDEBUG:\n" ++ concat (reverse tbuff)
        tr = ""
        out = out' . (++ tr)
-}
        
-- | Format graph and return additional information
formatGraphDiag ::
    RDFGraph -> (ShowS,NodeGenLookupMap,Int,[String])
formatGraphDiag gr = (out,nodeMap ngs,nodeGen ngs,traceBuf fgs)
    where
        (out,fgs) = formatGraphDiag1 "\n" True emptyLookupMap gr
        ngs       = nodeGenSt fgs

--  Internal function starts with supplied prefix table and indent string,
--  and returns final state and formatted string.
--  This is provided for diagnostic access to the final state
formatGraphDiag1 :: String -> Bool -> NamespaceMap -> RDFGraph -> (ShowS,N3FormatterState)
formatGraphDiag1 ind dopref pref gr = 
    let fg = formatGraph ind " ." False dopref gr
        ngs = emptyNgs {
                prefixes=pref,
                nodeGen=findMaxBnode gr
              }
             
    in runState fg (emptyN3FS ngs)

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

-- ind      is indentation string
-- end      is ending string to be placed after final statement
-- dobreak  is True if a line break is to be inserted at the start
-- dopref   is True if prefix strings are to be generated
--
formatGraph :: String -> String -> Bool -> Bool -> RDFGraph -> Formatter ShowS
formatGraph ind end dobreak dopref gr = do
  setIndent ind
  setLineBreak dobreak
  setGraph gr
  fp <- if dopref
        then formatPrefixes (getNamespaces gr)
        else return $ puts ""
  more <- moreSubjects
  res  <- if more
          then do
            fr <- formatSubjects
            return $ fp . fr . puts end
          else return fp

  return res

formatPrefixes :: NamespaceMap -> Formatter ShowS
formatPrefixes pmap = do
  let mls = map (pref . keyVal) (listLookupMap pmap)
  ls <- sequence mls
  return $ puts $ concat ls
    where
      pref (p,u) = nextLine $ "@prefix "++p++": <"++ quote True u ++"> ."

formatSubjects :: Formatter ShowS
formatSubjects = do
  sb    <- nextSubject
  sbstr <- formatLabel SubjContext sb
  
  flagP <- moreProperties
  if flagP
    then do
      prstr <- formatProperties sb sbstr
      fmstr <- formatFormulae ""
      flagS <- moreSubjects
      if flagS
        then do
          fr <- formatSubjects
          return $ puts (prstr ++ fmstr ++ " .") . fr
        else return $ puts $ prstr ++ fmstr
           
    else do
         txt <- nextLine sbstr
         return $ puts txt
    
formatProperties :: RDFLabel -> String -> Formatter String
formatProperties sb sbstr = do
  pr <- nextProperty sb
  prstr <- formatLabel PredContext pr
  obstr <- formatObjects sb pr (sbstr++" "++prstr)
  more  <- moreProperties
  let sbindent = replicate (length sbstr) ' '
  if more
    then do
      fr <- formatProperties sb sbindent
      nl <- nextLine $ obstr ++ " ;"
      return $ nl ++ fr
    else nextLine obstr

formatObjects :: RDFLabel -> RDFLabel -> String -> Formatter String
formatObjects sb pr prstr = do
  ob    <- nextObject sb pr
  obstr <- formatLabel ObjContext ob
  more  <- moreObjects
  if more
    then do
      let prindent = replicate (length prstr) ' '
      fr <- formatObjects sb pr prindent
      nl <- nextLine $ prstr ++ " " ++ obstr ++ ","
      return $ nl ++ fr
    else return $ prstr ++ " " ++ obstr

formatFormulae :: String -> Formatter String
formatFormulae fp = do
  more  <- moreFormulae
  if more
    then do
      fnlgr <- nextFormula
      fnstr <- formatFormula fnlgr
      formatFormulae $ fp ++ " ." ++ fnstr
    else return fp

{-
TODO: need to remove the use of :-. It's not clear to me whether
we are guaranteed that fn is only used once in the graph - ie
if it is safe to inline this formula at the label location.
-}

formatFormula :: (RDFLabel,RDFGraph) -> Formatter String
formatFormula (fn,gr) = do
  fnstr <- formatLabel SubjContext fn
  f1str <- nextLine $ fnstr ++ " :-"
  f2str <- nextLine "    {"
  ngs0  <- getNgs
  ind   <- getIndent
  let grm = formatGraph (ind++"    ") "" True False
            (setNamespaces emptyNamespaceMap gr)
            
      (f3str, fgs') = runState grm (emptyN3FS ngs0)

  setNgs (nodeGenSt fgs')
  f4str <- nextLine "    }"
  return $ f1str ++ f2str ++ f3str f4str

--- DJB's version of formatFormula when it can be inserted inline
insertFormula :: RDFGraph -> Formatter String
insertFormula gr = do
  ngs0  <- getNgs
  ind   <- getIndent
  let grm = formatGraph (ind++"    ") "" True False
            (setNamespaces emptyNamespaceMap gr)

      (f3str, fgs') = runState grm (emptyN3FS ngs0)

  setNgs (nodeGenSt fgs')
  f4str <- nextLine " } "
  return $ " { " ++ f3str f4str

{-
Add a list inline. We are given the labels that constitute
the list, in order, so just need to display them surrounded
by ().
-}
insertList :: [RDFLabel] -> Formatter String
insertList [] = return $ "()" -- not convinced this can happen
insertList xs = do
  ls <- mapM (formatLabel ObjContext) xs
  return $ "( " ++ intercalate " " ls ++ " )"
  
  
{-
Add a blank node inline.
-}

insertBnode :: LabelContext -> RDFLabel -> Formatter String  
insertBnode SubjContext lbl = do
  flag <- moreProperties
  txt <- if flag
         then liftM (++"\n") $ formatProperties lbl ""
         else return ""

  -- TODO: handle indentation?
  return $ "[" ++ txt ++ "]"

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
         then liftM (++"\n") $ formatProperties lbl ""
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
  return $ "[" ++ txt ++ "]"
  
----------------------------------------------------------------------
--  Formatting helpers
----------------------------------------------------------------------

setGraph        :: RDFGraph -> Formatter ()
setGraph gr = do
  st <- get

  let ngs0 = nodeGenSt st
      pre' = mapMerge (prefixes ngs0) (getNamespaces gr)
      ngs' = ngs0 { prefixes = pre' }
      arcs = sortArcs $ getArcs gr
      nst  = st  { graph     = gr
                 , subjs     = arcTree arcs
                 , props     = []
                 , objs      = []
                 , formAvail = getFormulae gr
                 , nodeGenSt = ngs'
                 , bNodesCheck   = countBnodes arcs
                 }

  put nst

moreSubjects    :: Formatter Bool
moreSubjects    = (not . null . subjs) `liftM` get

nextSubject     :: Formatter RDFLabel
nextSubject     = do
  st <- get

  let sb:sbs = subjs st
      nst = st  { subjs = sbs
                , props = snd sb
                , objs  = []
                }

  put nst
  return $ fst sb

moreProperties  :: Formatter Bool
moreProperties  = (not . null . props) `liftM` get

nextProperty    :: RDFLabel -> Formatter RDFLabel
nextProperty _ = do
  st <- get

  let pr:prs = props st
      nst = st  { props = prs
                 , objs  = snd pr
                 }

  put nst
  return $ fst pr


moreObjects     :: Formatter Bool
moreObjects     = (not . null . objs) `liftM` get

nextObject      :: RDFLabel -> RDFLabel -> Formatter RDFLabel
nextObject _ _ = do
  st <- get

  let ob:obs = objs st
      nst = st { objs = obs }

  put nst
  return ob

nextLine        :: String -> Formatter String
nextLine str = do
  ind <- getIndent
  brk <- getLineBreak
  if brk
    then return $ ind++str
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
--
--  [[[TODO:]]]
--  (d) generate multi-line literals when appropriate
--
-- This is being updated to produce inline formula, lists and     
-- blank nodes. The code is not efficient.
--

specialTable :: [(ScopedName, String)]
specialTable = 
  [ (rdf_type, "a")
  , (owl_sameAs, "=")
  , (log_implies, "=>")
  , (rdf_nil, "()")
  ]
  
formatLabel :: LabelContext -> RDFLabel -> Formatter String
{-
formatLabel lab@(Blank (_:_)) = do
  name <- formatNodeId lab
  queueFormula lab
  return name
-}

{-
The "[..]" conversion is done last, after "()" and "{}" checks.
-}
formatLabel lctxt lab@(Blank (_:_)) = do
  mlst <- extractList lctxt lab
  case mlst of
    Just lst -> insertList lst
    Nothing -> do
              mfml <- extractFormula lab
              case mfml of
                Just fml -> insertFormula fml
                Nothing -> do
                          nb1 <- getBnodesCheck
                          if lctxt /= PredContext && lab `notElem` nb1
                            then insertBnode lctxt lab
                            else formatNodeId lab

formatLabel _ lab@(Res sn) = 
  case lookup sn specialTable of
    Just txt -> return $ quote True txt -- TODO: do we need to quote?
    Nothing -> do
      pr <- getPrefixes
      let nsuri  = getScopeURI sn
          local  = snLocal sn
          premap = reverseLookupMap pr :: RevNamespaceMap
          prefix = mapFindMaybe nsuri premap
          name   = case prefix of
                     Just p -> quote True (p ++ ":" ++ local) -- TODO: what are quoting rules for QNames
                     _ -> "<"++ quote True (nsuri++local) ++">"
      queueFormula lab
      return name

formatLabel _ (Lit lit mlit) = return $ quoteStr lit ++ formatAnnotation mlit

formatLabel _ lab = return $ show lab

-- the annotation for a literal (ie type or language)
formatAnnotation :: Maybe ScopedName -> String
formatAnnotation Nothing = ""
formatAnnotation (Just a)  | isLang a  = '@' : langTag a
                           | otherwise = '^':'^': showScopedName a

{-
Swish.HaskellUtils.MiscHelpers contains a quote routine
which we expand upon here to match the N3 syntax.

We have to decide whether to use " or """ to quote
the string.

There is also no need to restrict the string to the
ASCII character set; this could be an option but we
can also leave Unicode as is (or at least convert to UTF-8).
-}

quoteStr :: String -> String
quoteStr st = 
  let qst = quote (n==1) st
      n = if '\n' `elem` st || '"' `elem` st then 3 else 1
      qch = replicate n '"'                              
  in qch ++ qst ++ qch

-- if the first element is True then we need to 
-- quote " and new lines.
--
quote :: Bool -> String -> String
quote _     []           = ""
quote True  ('"': st)    = '\\':'"': quote True  st
quote True  ('\n':st)    = '\\':'n': quote True  st
quote True  ('\t':st)    = '\\':'t': quote True  st
quote False ('"': st)    =      '"': quote False st
quote False ('\n':st)    =     '\n': quote False st
quote False ('\t':st)    =     '\t': quote False st
quote f ('\r':st)    = '\\':'r': quote f st
quote f ('\\':st)    = '\\':'\\': quote f st -- not sure about this
quote f (c:st) = 
  let nc = ord c
      rst = quote f st
      
      -- lazy way to convert to a string
      hstr = printf "%08X" nc
      ustr = hstr ++ rst

  in if nc > 0xffff 
     then '\\':'U': ustr
     else if nc > 0x7e || nc < 0x20
          then '\\':'u': drop 4 ustr
          else c : rst
                      
formatNodeId :: RDFLabel -> Formatter String
formatNodeId lab@(Blank (lnc:_)) =
    if isDigit lnc then mapBlankNode lab else return $ show lab
formatNodeId other = error $ "formatNodeId not expecting a " ++ show other -- to shut up -Wall

mapBlankNode :: RDFLabel -> Formatter String
mapBlankNode lab = do
  ngs <- getNgs
  let cmap = nodeMap ngs
      cval = nodeGen ngs
  nv <- case mapFind 0 lab cmap of
    0 -> do 
      let nval = succ cval
          nmap = mapAdd cmap (lab, nval)
      setNgs $ ngs { nodeGen = nval, nodeMap = nmap }
      return nval
      
    n -> return n
  
  -- TODO: is this what we want?
  return $ "_:swish" ++ show nv

-- TODO: need to be a bit more clever with this than we did in NTriples
--       not sure the following counts as clever enough ...
--  
showScopedName :: ScopedName -> String
{-
showScopedName (ScopedName n l) = 
  let uri = nsURI n ++ l
  in quote uri
-}
showScopedName = quote True . show

----------------------------------------------------------------------
--  Graph-related helper functions
----------------------------------------------------------------------

newtype SortedArcs lb = SA [Arc lb]

sortArcs :: (Ord lb) => [Arc lb] -> SortedArcs lb
sortArcs = SA . stableQuickSort

--  Rearrange a list of arcs into a tree of pairs which group together
--  all statements for a single subject, and similarly for multiple
--  objects of a common predicate.
--
arcTree :: (Eq lb) => SortedArcs lb -> SubjTree lb
arcTree (SA as) = commonFstEq (commonFstEq id) $ map spopair as
    where
        spopair (Arc s p o) = (s,(p,o))

{-
arcTree as = map spopair $ sort as
    where
        spopair (Arc s p o) = (s,[(p,[o])])
-}

--  Rearrange a list of pairs so that multiple occurrences of the first
--  are commoned up, and the supplied function is applied to each sublist
--  with common first elements to obtain the corresponding second value
commonFstEq :: (Eq a) => ( [b] -> c ) -> [(a,b)] -> [(a,c)]
commonFstEq f ps =
    [ (fst $ head sps,f $ map snd sps) | sps <- groupBy fstEq ps ]
    where
        fstEq (f1,_) (f2,_) = f1 == f2

{-
-- Diagnostic code for checking arcTree logic:
testArcTree = (arcTree testArcTree1) == testArcTree2
testArcTree1 =
    [Arc "s1" "p11" "o111", Arc "s1" "p11" "o112"
    ,Arc "s1" "p12" "o121", Arc "s1" "p12" "o122"
    ,Arc "s2" "p21" "o211", Arc "s2" "p21" "o212"
    ,Arc "s2" "p22" "o221", Arc "s2" "p22" "o222"
    ]
testArcTree2 =
    [("s1",[("p11",["o111","o112"]),("p12",["o121","o122"])])
    ,("s2",[("p21",["o211","o212"]),("p22",["o221","o222"])])
    ]
-}


findMaxBnode :: RDFGraph -> Int
findMaxBnode = maximum . map getAutoBnodeIndex . labels

getAutoBnodeIndex   :: RDFLabel -> Int
getAutoBnodeIndex (Blank ('_':lns)) = res where
    -- cf. prelude definition of read s ...
    res = case [x | (x,t) <- reads lns, ("","") <- lex t] of
            [x] -> x
            _   -> 0
getAutoBnodeIndex _                   = 0

{-
Find all blank nodes that occur
  - any number of times as a subject
  - 0 or 1 times as an object

Such nodes can be output using the "[..]" syntax. To make it simpler
to check we actually store those nodes that can not be expanded.

Note that we do not try and expand any bNode that is used in
a predicate position.

Should probably be using the SubjTree RDFLabel structure but this
is easier for now.

-}

countBnodes :: SortedArcs RDFLabel -> [RDFLabel]
countBnodes (SA as) = snd (foldl' ctr ([],[]) as)
    where
      -- first element of tuple are those blank nodes only seen once,
      -- second element those blank nodes seen multiple times
      --
      inc b@(b1s,bms) l@(Blank _) | l `elem` bms = b
                                  | l `elem` b1s = (delete l b1s, l:bms)
                                  | otherwise    = (l:b1s, bms)
      inc b _ = b

      -- if the bNode appears as a predicate we instantly add it to the
      -- list of nodes not to expand, even if only used once
      incP b@(b1s,bms) l@(Blank _) | l `elem` bms = b
                                   | l `elem` b1s = (delete l b1s, l:bms)
           			   | otherwise    = (b1s, l:bms)
      incP b _ = b

      ctr orig (Arc _ p o) = inc (incP orig p) o

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
