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

*) list syntax

There are times when the first element of a list is not being
recognized as such:

exoticN3Graph_x5 =
    commonPrefixes ++
    " (base1:o1 base2:o2 base3:o3 \"l1\") = base1:s1 .\n"

which we output as

  _:b1 rdf:first base1:o1 ;
        rdf:rest ( base2:o2 base3:o3 "l1" ) ;
        = base1:s1 .

also

@prefix : <urn:base#> .
:xx rdf:first :a ;
   rdf:rest _:_1 .
_:_1 rdf:first :b ;
     rdf:rest _:_2 .
_:_2 rdf:first :c ;
     rdf:rest _:_3 .
_:_3 rdf:first "lbl" ;
     rdf:rest rdf:nil .

to

@prefix : <urn:base#> .
:xx rdf:first :a ;
    rdf:rest ( :b :c "lbl" ) .

Is this happening in formatSubjects/formatProperties?

I think so, because extractList requires that the label be
in the SubjTree, but for the first element it isn't, as shown below
where the osubjs list for 'extractList _:1' has no entry for _:1

@prefix foo: <urn:foo#> .
_:_1 rdf:first foo:a ;
     rdf:rest ( foo:q ) ;
     = foo:b .
DEBUG:
extractList _:1
 -> osubjs= [(_:2,[(rdf:first,[foo:q]),(rdf:rest,[rdf:nil])])]
 -> mlst= Nothing
extractList rdf:first
 -> osubjs= [(_:2,[(rdf:first,[foo:q]),(rdf:rest,[rdf:nil])])]
 -> mlst= Nothing
extractList foo:a
 -> osubjs= [(_:2,[(rdf:first,[foo:q]),(rdf:rest,[rdf:nil])])]
 -> mlst= Nothing
extractList rdf:rest
 -> osubjs= [(_:2,[(rdf:first,[foo:q]),(rdf:rest,[rdf:nil])])]
 -> mlst= Nothing
extractList _:2
 -> osubjs= [(_:2,[(rdf:first,[foo:q]),(rdf:rest,[rdf:nil])])]
 -> mlst= Just ([foo:q],[_:2])
extractList foo:q
 -> osubjs= []
 -> mlst= Nothing
extractList foo:b
 -> osubjs= []
 -> mlst= Nothing


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
  isUri, isBlank,
  res_rdf_first, res_rdf_rest, res_rdf_nil
  )

import Swish.HaskellRDF.Vocabulary (
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

import Data.Char (isDigit)

import Data.List (groupBy, intercalate)

import Data.Maybe (fromMaybe)

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

data Fgs = Fgs
    { indent    :: String
    , lineBreak :: Bool
    , graph     :: RDFGraph
    , subjs     :: SubjTree RDFLabel
    , props     :: PredTree RDFLabel   -- for last subject selected
    , objs      :: [RDFLabel]          -- for last property selected
    , formAvail :: FormulaMap RDFLabel
    , formQueue :: [(RDFLabel,RDFGraph)]
    , nodeGenSt :: NodeGenState
    , traceBuf  :: [String]
    }

emptyFgs :: NodeGenState -> Fgs
emptyFgs ngs = Fgs
    { indent    = "\n"
    , lineBreak = False
    , graph     = emptyRDFGraph
    , subjs     = []
    , props     = []
    , objs      = []
    , formAvail = emptyFormulaMap
    , formQueue = []
    , nodeGenSt = ngs
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

--  monad definition adapted from Simon Thompson's book, p410
--
--  Fgsm a is a "state transformer" on a state of type "Fgs",
--  which additionally returns a value of type 'a'.
data Fgsm a = Fgsm ( Fgs -> (Fgs,a) )

instance Monad Fgsm where
    return res      = Fgsm (\fgs -> (fgs,res))
    (Fgsm st) >>= f = Fgsm (\fgs ->
        let (newfgs,res) = st fgs
            (Fgsm st')   = f res
        in
            st' newfgs
        )

getFgs :: Fgsm Fgs
getFgs = Fgsm (\fgs -> (fgs,fgs) )

getIndent :: Fgsm String
getIndent = Fgsm (\fgs -> (fgs,indent fgs) )

setIndent :: String -> Fgsm ()
setIndent ind = Fgsm (\fgs -> (fgs {indent=ind},()) )

getLineBreak :: Fgsm Bool
getLineBreak = Fgsm (\fgs -> (fgs,lineBreak fgs) )

setLineBreak :: Bool -> Fgsm ()
setLineBreak brk = Fgsm (\fgs -> (fgs {lineBreak=brk},()) )

getNgs :: Fgsm NodeGenState
getNgs = Fgsm (\fgs -> (fgs,nodeGenSt fgs) )

setNgs :: NodeGenState -> Fgsm ()
setNgs ngs = Fgsm (\fgs -> (fgs { nodeGenSt = ngs },()) )

getPrefixes :: Fgsm NamespaceMap
getPrefixes = Fgsm (\fgs -> (fgs,prefixes (nodeGenSt fgs)) )

getProps :: Fgsm (PredTree RDFLabel)
getProps = Fgsm $ \fgs -> (fgs, props fgs)

setProps :: PredTree RDFLabel -> Fgsm ()
setProps ps = Fgsm $ \fgs -> (fgs { props = ps }, ())

getObjs :: Fgsm ([RDFLabel])
getObjs = Fgsm $ \fgs -> (fgs, objs fgs)

setObjs :: [RDFLabel] -> Fgsm ()
setObjs os = Fgsm $ \fgs -> (fgs { objs = os }, ())

addTrace :: String -> Fgsm ()
addTrace tr = Fgsm $ \fgs -> (fgs { traceBuf = tr : traceBuf fgs }, ())
  
queueFormula :: RDFLabel -> Fgsm ()
queueFormula fn = Fgsm (\fgs ->
    let fa = formAvail fgs
        newState fv =
            fgs { formAvail=mapDelete fa fn
                , formQueue=(fn,fv) : formQueue fgs
                }
    in
        case mapFindMaybe fn fa of
            Nothing -> (fgs,())
            Just fv -> (newState fv,())
    )

{-
Return the graph associated with the label and delete it
from the store, if there is an association, otherwise
return Nothing.
-}
extractFormula :: RDFLabel -> Fgsm (Maybe RDFGraph)
extractFormula fn = Fgsm $ \fgs ->
  let fa = formAvail fgs
      newState = fgs { formAvail=mapDelete fa fn }
  in
   case mapFindMaybe fn fa of
     Nothing -> (fgs, Nothing)
     Just fv -> (newState,Just fv)
  
moreFormulae :: Fgsm Bool
moreFormulae =  Fgsm (\fgs -> (fgs,not $ null (formQueue fgs)) )

nextFormula :: Fgsm (RDFLabel,RDFGraph)
nextFormula =  Fgsm (\fgs ->
    let (nf:fq) = (formQueue fgs) in (fgs {formQueue=fq},nf)
    )

{-
The label contains a list if

  label rdf:first foo
  label rdf:rest bar

and ditto for bar until end up with rdf:nil.
Is this sufficient or in fact necessary?

Note: I originally checked that label did not have any other
predicates associated with it, but then you can not have

  (:a :b) = :c

since this becomes

 _:n1 rdf:first :a ; rdf:next _:n2 ; a :c .

This is very inefficient.

Trying to work out why ( ... ) = ... does not get round-tripped correctly.

*Swish.HaskellRDF.N3Parser> parseN3 "@prefix b1: <urn:b1#>. () = b1:x." Nothing
Right Graph, formulae: 
arcs: 
    (rdf:nil,owl:sameAs,b1:x)
*Swish.HaskellRDF.N3Parser> parseN3 "@prefix b1: <urn:b1#>. ( b1:y ) = b1:x." Nothing
Right Graph, formulae: 
arcs: 
    (_:1,owl:sameAs,b1:x)
    (_:1,rdf:rest,rdf:nil)
    (_:1,rdf:first,b1:y)
*Swish.HaskellRDF.N3Parser> parseN3 "@prefix b1: <urn:b1#>. ( b1:y b1:z ) = b1:x." Nothing
Right Graph, formulae: 
arcs: 
    (_:1,owl:sameAs,b1:x)
    (_:2,rdf:rest,rdf:nil)
    (_:2,rdf:first,b1:z)
    (_:1,rdf:rest,_:2)
    (_:1,rdf:first,b1:y)

-}

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
extractList :: RDFLabel -> Fgsm (Maybe [RDFLabel])
extractList ln = Fgsm $ \fgs -> 
  let osubjs = subjs fgs
      mlst = getCollection osubjs ln
      tr = "extractList " ++ show ln ++ "\n -> osubjs= " ++ show osubjs ++ "\n -> mlst= " ++ show mlst ++ "\n"
      fgs' = fgs { traceBuf = tr : traceBuf fgs }
  in case mlst of
    Just (sl,ls,_) -> (fgs' { subjs = sl }, Just ls)
    Nothing        -> (fgs', Nothing)
  
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

{-
Return the arcs associated with the label and delete them
from the store.

Do we need to worry about any formulae?
-}

{-
extractBNode :: RDFLabel -> Fgsm RDFGraph
extractBNode bn = Fgsm $ \fgs ->
  let osubjs = subjs fgs
      opreds = preds fgs
      oobjs  = objs fgs
      ongst  = nodeGenSt fgs
XXXX what do we need to copy over to the new graph and what do we      
need to return?

Hmm, maybe having an extraction and a insertion step for BNode isn't
ideal?

  in
   case mapFindMaybe fn fa of
     Nothing -> (fgs, Nothing)
     Just fv -> (newState,Just fv)
-}

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
formatGraphIndent ind dopref gr = out
    where
        (fgs,out') = formatGraphDiag1 ind dopref emptyLookupMap gr
        tbuff = traceBuf fgs
        -- tr = if null tbuff then "" else "\nDEBUG:\n" ++ concat (reverse tbuff)
        tr = ""
        out = out' . (++ tr)
        
-- | Format graph and return additional information
formatGraphDiag ::
    RDFGraph -> (ShowS,NodeGenLookupMap,Int,[String])
formatGraphDiag gr = (out,nodeMap ngs,nodeGen ngs,traceBuf fgs)
    where
        (fgs,out) = formatGraphDiag1 "\n" True emptyLookupMap gr
        ngs       = nodeGenSt fgs

--  Internal function starts with supplied prefix table and indent string,
--  and returns final state and formatted string.
--  This is provided for diagnostic access to the final state
formatGraphDiag1 :: String -> Bool -> NamespaceMap -> RDFGraph -> (Fgs,ShowS)
formatGraphDiag1 ind dopref pref gr = res where
    Fgsm fg = formatGraph ind " ." False dopref gr  -- construct monad
    ngs     = emptyNgs                  -- construct initial state
                { prefixes=pref
                , nodeGen=findMaxBnode gr
                }
    (_,res) = fg (emptyFgs ngs)         -- apply monad to state, pick result

----------------------------------------------------------------------
--  Formatting as a monad-based computation
----------------------------------------------------------------------

-- ind      is indentation string
-- end      is ending string to be placed after final statement
-- dobreak  is True if a line break is to be inserted at the start
-- dopref   is True if prefix strings are to be generated
--
formatGraph :: String -> String -> Bool -> Bool -> RDFGraph -> Fgsm (Fgs,ShowS)
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
  fgs <- getFgs
  return (fgs,res)

formatPrefixes :: NamespaceMap -> Fgsm ShowS
formatPrefixes pmap = do
  let mls = map (pref . keyVal) (listLookupMap pmap)
  ls <- sequence mls
  return $ puts $ concat ls
    where
      pref (p,u) = nextLine $ "@prefix "++p++": <"++u++"> ."

--  The above function creates a list of 'Fgsm String' monads, then
--  uses 'sequence' to turn that to a single 'Fgsm [String]' and finally
--  concatenates them to a single string and uses 'puts' to return the
--  result as a 'Fgsm ShowS'.  Phew!

formatSubjects :: Fgsm ShowS
formatSubjects = do
  sb    <- nextSubject
  sbstr <- formatLabel sb
  prstr <- formatProperties sb sbstr
  fmstr <- formatFormulae ""
  more  <- moreSubjects
  if more
    then do
      fr <- formatSubjects
      return $ puts (prstr ++ fmstr ++ " .") . fr
    else return $ puts $ prstr ++ fmstr

{-
We special case lists here, relying on the lexical ordering
applied in the creation of the property list so we assume that
rdf:first will be processed before rdf:next.

Now, since we now do NOT want to convert

  named-node rdf:first ...

into 

  named-node owl:sameAs (..)

then is all this needed?

It also needs some clean up.

-}
formatProperties :: RDFLabel -> String -> Fgsm String
formatProperties sb sbstr = do
  pr <- nextProperty sb
  if isBlank sb && pr == res_rdf_first
    then do
      -- addTrace $ "In formatProperties and have a blank node <" ++ show sb ++ "> rdf:first ...\n"
      pList <- getProps
      let mnext = removeItem pList res_rdf_rest
      -- addTrace $ " -> pList=" ++ show pList ++ "\n"
      -- addTrace $ " -> removeItem -> " ++ show mnext ++ "\n"
      case mnext of
        Just ([nextObj], pList') -> do
          -- TODO: if sbstr is not empty then force pList' to be empty, I think
          --
          -- if we get this far then there must be a res_rdf_first in here
          -- we want to make sure that there's only one object pointed to by rdf:first

          headObjs <- getObjs
          case headObjs of
            [headObj] -> do
          
              -- addTrace $ "List head; subject node = " ++ show sb ++ " [" ++ sbstr ++ "]\n"

              mlst <- extractList nextObj
	      case mlst of
	        Nothing -> formatProperties1 sb sbstr res_rdf_first
                Just lst -> do
                  obstr <- insertList $ headObj : lst
                  setProps pList'
                  setObjs [] -- HMM, TODO check since don't we want to combine with setProps properly...
		  -- although it pList' is supposed to be [] then it is technically true...
                  more <- moreProperties
                  if more
                    then do
                      -- need to work out indenting if blank node vs named node
                      let sbindent = replicate (length sbstr) ' '
                      fr <- formatProperties sb sbindent
                      nl <- nextLine $ obstr ++ " "
                      return $ nl ++ fr
                    else nextLine $ obstr
                 
            _ -> formatProperties1 sb sbstr res_rdf_first

        _ -> formatProperties1 sb sbstr res_rdf_first
    
    else formatProperties1 sb sbstr pr
         
{-         
TODO:
Can we use RDFGraph.isMemberProp to check for list/anonymous node?

This is RDF Containers (rdf:Seq rdf:Bag rdf:Alt) and not Collections
(well, actually not 100% clear from the documentation).
-}

formatProperties1 :: RDFLabel -> String -> RDFLabel -> Fgsm String
formatProperties1 sb sbstr pr = do
  prstr <- formatLabel pr
  obstr <- formatObjects sb pr (sbstr++" "++prstr)
  more  <- moreProperties
  let sbindent = replicate (length sbstr) ' '
  if more
    then do
      fr <- formatProperties sb sbindent
      nl <- nextLine $ obstr ++ " ;"
      return $ nl ++ fr
    else nextLine obstr

formatObjects :: RDFLabel -> RDFLabel -> String -> Fgsm String
formatObjects sb pr prstr = do
  ob    <- nextObject sb pr
  obstr <- formatLabel ob
  more  <- moreObjects
  if more
    then do
      let prindent = replicate (length prstr) ' '
      fr <- formatObjects sb pr prindent
      nl <- nextLine $ prstr ++ " " ++ obstr ++ ","
      return $ nl ++ fr
    else return $ prstr ++ " " ++ obstr

formatFormulae :: String -> Fgsm String
formatFormulae fp = do
  more  <- moreFormulae
  if more
    then do
      fnlgr <- nextFormula
      fnstr <- formatFormula fnlgr
      formatFormulae $ fp ++ " ." ++ fnstr
    else return fp

-- [[[TODO: use above pattern for subject/property/object loops?]]]

{-
TODO: need to remove the use of :-. It's not clear to me whether
we are guaranteed that fn is only used once in the graph - ie
if it is safe to inline this formula at the label location.
-}

formatFormula :: (RDFLabel,RDFGraph) -> Fgsm String
formatFormula (fn,gr) = do
  fnstr <- formatLabel fn
  f1str <- nextLine $ fnstr ++ " :-"
  f2str <- nextLine "    {"
  ngs0  <- getNgs
  ind   <- getIndent
  let Fgsm grm = formatGraph (ind++"    ") "" True False
                             (setNamespaces emptyNamespaceMap gr)
  let (fgs',(_,f3str)) = grm (emptyFgs ngs0)
  setNgs (nodeGenSt fgs')
  f4str <- nextLine "    }"
  return $ f1str ++ f2str ++ f3str f4str

--- DJB's version of formatFormula when it can be inserted inline
insertFormula :: RDFGraph -> Fgsm String
insertFormula gr = do
  ngs0  <- getNgs
  ind   <- getIndent
  let Fgsm grm = formatGraph (ind++"    ") "" True False
                 (setNamespaces emptyNamespaceMap gr)
      (fgs',(_,f3str)) = grm (emptyFgs ngs0)
  setNgs (nodeGenSt fgs')
  f4str <- nextLine " } "
  return $ " { " ++ f3str f4str

{-
Add a list inline. We are given the labels that constitute
the list, in order, so just need to display them surrounded
by ().
-}
insertList :: [RDFLabel] -> Fgsm String
insertList [] = return $ "()" -- not convinced this can happen
insertList xs = do
  ls <- mapM formatLabel xs
  return $ "( " ++ intercalate " " ls ++ " )"
  
  
{-
Add a blank node inline, where the input is the
graph (and how about formula) to display.

insertBNode :: RDFGraph -> Fgsm String  
insertBNode gr = do
  undefined
-}
  
----------------------------------------------------------------------
--  Formatting helpers
----------------------------------------------------------------------

setGraph        :: RDFGraph -> Fgsm ()
setGraph gr =
    Fgsm (\fgs ->
        let ngs0 = (nodeGenSt fgs)
            pre' = mapMerge (prefixes ngs0) (getNamespaces gr)
            ngs' = ngs0 { prefixes=pre' }
            fgs' = fgs  { graph     = gr
                        , subjs     = arcTree $ getArcs gr
                        , props     = []
                        , objs      = []
                        , formAvail = getFormulae gr
                        , nodeGenSt = ngs'
                        }
        in (fgs',()) )

moreSubjects    :: Fgsm Bool
moreSubjects    = Fgsm (\fgs -> (fgs,not $ null (subjs fgs)))

nextSubject     :: Fgsm RDFLabel
nextSubject     =
    Fgsm (\fgs ->
        let sb:sbs = subjs fgs
            fgs' = fgs  { subjs = sbs
                        , props = snd sb
                        , objs  = []
                        }
        in (fgs',fst sb) )


moreProperties  :: Fgsm Bool
moreProperties  = Fgsm (\fgs -> (fgs,not $ null (props fgs)))

nextProperty    :: RDFLabel -> Fgsm RDFLabel
nextProperty _ =
    Fgsm (\fgs ->
        let pr:prs = props fgs
            fgs' = fgs  { props = prs
                        , objs  = snd pr
                        }
        in (fgs',fst pr) )


moreObjects     :: Fgsm Bool
moreObjects     = Fgsm (\fgs -> (fgs,not $ null (objs fgs)))

nextObject      :: RDFLabel -> RDFLabel -> Fgsm RDFLabel
nextObject _ _ =
    Fgsm (\fgs ->
        let ob:obs = objs fgs
            fgs'   = fgs { objs = obs }
        in (fgs',ob) )

nextLine        :: String -> Fgsm String
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
--  (d) blank nodes used just once, can be expanded inline using
--      [...] syntax.
--  (e) generate multi-line literals when appropriate
--
-- This is being updated to produce inline formula, lists and     
-- blank nodes. The code is not efficient.
--

-- modified from the version in N3Parser
specialTable :: [(ScopedName, String)]
specialTable = 
  [ (rdf_type, "a")
  , (owl_sameAs, "=")
  , (log_implies, "=>")
  , (rdf_nil, "()")
  ]
  
formatLabel :: RDFLabel -> Fgsm String
{-
formatLabel lab@(Blank (_:_)) = do
  name <- formatNodeId lab
  queueFormula lab
  return name
-}

formatLabel lab@(Blank (_:_)) = do
  mlst <- extractList lab
  case mlst of
    Just lst -> insertList lst
    Nothing -> do
      mfml <- extractFormula lab
      case mfml of
        Just fml -> insertFormula fml
        Nothing -> do
          -- gr <- extractBNode lab
          -- insertBNode gr
          formatNodeId lab

{-
TODO:
hmmm, we may not need list handling here? almost certainly don't so leave in the 'CC' version
so that it's obvious if this code is ever executed.
-}
formatLabel lab@(Res sn) = 
  case lookup sn specialTable of
    Just txt -> return txt
    Nothing -> do
      mlst <- extractList lab
      case mlst of
        -- Just lst -> insertList lst
        Just lst -> do
	  txt <- insertList lst
          return $ "<CC>" ++ txt ++ "<CC>"
        Nothing -> do
          pr <- getPrefixes
          let nsuri  = getScopeURI sn
              local  = snLocal sn
              premap = reverseLookupMap pr :: RevNamespaceMap
              prefix = mapFindMaybe nsuri premap
              name   = case prefix of
                Just p -> p ++ ":" ++ local
                _ -> "<"++nsuri++local++">"
          queueFormula lab
          return name

{-
formatLabel lab@(Lit str typ) =
    do  { return $ show lab
        }
formatLabel lab@(Var vid) =
    do  { return $ show lab
        }
-}
formatLabel lab = return $ show lab

formatNodeId :: RDFLabel -> Fgsm String
formatNodeId lab@(Blank (lnc:_)) =
    if isDigit lnc then mapBlankNode lab else return $ show lab
formatNodeId other = error $ "formatNodeId not expecting a " ++ show other -- to shut up -Wall

mapBlankNode :: RDFLabel -> Fgsm String
mapBlankNode lab = do
  ngs <- getNgs
  let nmap = nodeMap ngs
  nval <- case mapFind 0 lab nmap of
    0 -> do 
      let nn = nodeGen ngs + 1
          nm = mapAdd nmap (lab,nn)
      setNgs $ ngs { nodeGen=nn, nodeMap=nm }
      return nn
    n -> return n
  
  return $ show $ Blank ('_':show nval)

----------------------------------------------------------------------
--  Graph-related helper functions
----------------------------------------------------------------------

--  Rearrange a list of arcs into a tree of pairs which group together
--  all statements for a single subject, sind similarly for multiple
--  objects of a common predicate.
arcTree :: (Ord lb) => [Arc lb] -> SubjTree lb
arcTree as = commonFstEq (commonFstEq id) $ map spopair $ stableQuickSort as
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
