--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Query
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines functions for querying an RDF graph to obtain
--  a set of variable substitutions, and to apply a set of variable
--  substitutions to a query pattern to obtain a new graph.
--
--  It also defines a few primitive graph access functions.
--
--  A minimal example is shown below, where we query a very simple
--  graph:
--
-- >>> :m + Swish.RDF Swish.RDF.Parser.N3 Swish.RDF.Query
-- >>> :set -XOverloadedStrings
-- >>> let qparse = either error id . parseN3fromText
-- >>> let igr = qparse "@prefix a: <http://example.com/>. a:a a a:A ; a:foo a:bar. a:b a a:B ; a:foo a:bar."
-- >>> let qgr = qparse "?node a ?type."
-- >>> rdfQueryFind qgr igr
-- [[(?type,a:B),(?node,a:b)],[(?type,a:A),(?node,a:a)]]
-- >>> let bn = (toRDFLabel . Data.Maybe.fromJust . Network.URI.parseURI) "http://example.com/B"
-- >>> rdfFindArcs (rdfObjEq bn) igr
-- [(a:b,rdf:type,a:B)]
-- >>> Data.Maybe.mapMaybe (flip Swish.RDF.VarBinding.vbMap (Var "type")) $ rdfQueryFind qgr igr
-- [a:B,a:A]
-- 
--------------------------------------------------------------------------------

module Swish.RDF.Query
    ( rdfQueryFind, rdfQueryFilter
    , rdfQueryBack, rdfQueryBackFilter, rdfQueryBackModify
    , rdfQueryInstance
    , rdfQuerySubs, rdfQueryBackSubs
    , rdfQuerySubsAll
    , rdfQuerySubsBlank, rdfQueryBackSubsBlank
    , rdfFindArcs, rdfSubjEq, rdfPredEq, rdfObjEq
    , rdfFindPredVal, rdfFindPredInt, rdfFindValSubj
    , rdfFindList
    -- * Exported for testing
    , rdfQuerySubs2 )
where

import Swish.Datatype (DatatypeMap(..))
import Swish.VarBinding (VarBinding(..), VarBindingModify(..), VarBindingFilter(..))
import Swish.VarBinding (makeVarBinding, applyVarBinding, joinVarBindings)

import Swish.RDF.Graph
    ( Arc(..), LDGraph(..)
    , arcSubj, arcPred, arcObj
    , RDFLabel(..)
    , isDatatyped, isBlank, isQueryVar
    , getLiteralText, makeBlank
    , RDFTriple
    , RDFGraph, emptyRDFGraph
    , allLabels, remapLabels
    , resRdfFirst
    , resRdfRest
    , resRdfNil
    )

import Swish.RDF.VarBinding (RDFVarBinding, RDFVarBindingFilter)
import Swish.RDF.VarBinding (nullRDFVarBinding)

import Swish.RDF.Datatype.XSD.MapInteger (mapXsdInteger)

import Swish.RDF.Vocabulary (xsdInteger, xsdNonNegInteger)

import Swish.Utils.ListHelpers (listProduct, allp, anyp)

import qualified Data.Traversable as Traversable

import Control.Monad (when)
import Control.Monad.State (State, runState, modify)

import Data.Maybe (mapMaybe, isJust, fromJust)

import qualified Data.Set as S

-- import qualified Data.Text as T

------------------------------------------------------------
--  Primitive RDF graph queries
------------------------------------------------------------

-- | Basic graph-query function.
--
--  The triples of the query graph are matched sequentially
--  against the target graph, each taking account of any
--  variable bindings that have already been determined,
--  and adding new variable bindings as triples containing
--  query variables are matched against the graph.
--
rdfQueryFind :: 
  RDFGraph -- ^ The query graph.
  -> RDFGraph -- ^ The target graph.
  -> [RDFVarBinding]
  -- ^ Each element represents a set of variable bindings that make the query graph a
  -- subgraph of the target graph. The list can be empty.
rdfQueryFind =
    rdfQueryPrim1 matchQueryVariable nullRDFVarBinding . getArcs

--  Helper function to match query against a graph.
--  A node-query function is supplied to determine how query nodes
--  are matched against target graph nodes.  Also supplied is
--  an initial variable binding.
--
rdfQueryPrim1 ::
    NodeQuery RDFLabel -> RDFVarBinding -> [Arc RDFLabel]
    -> RDFGraph
    -> [RDFVarBinding]
rdfQueryPrim1 _     initv []       _  = [initv]
rdfQueryPrim1 nodeq initv (qa:qas) tg =
    let
        qam  = fmap (applyVarBinding initv) qa      -- subst vars already bound
        newv = rdfQueryPrim2 nodeq qam tg           -- new bindings, or null
    in
        concat
            [ rdfQueryPrim1 nodeq v2 qas tg
            | v1 <- newv
            , let v2 = joinVarBindings initv v1
            ]

--  Match single query term against graph, and return any new sets
--  of variable bindings thus defined, or [] if the query term
--  cannot be matched.  Each of the RDFVarBinding values returned
--  represents an alternative possible match for the query arc.
--
rdfQueryPrim2 ::
    NodeQuery RDFLabel -> Arc RDFLabel
    -> RDFGraph
    -> [RDFVarBinding]
rdfQueryPrim2 nodeq qa tg =
        mapMaybe (getBinding nodeq qa) (getArcs tg)

-- |RDF query filter.
--
--  This function applies a supplied query binding
--  filter to the result from a call of 'rdfQueryFind'.
--
--  If none of the query bindings found satisfy the filter, a null
--  list is returned (which is what 'rdfQueryFind' returns if the
--  query cannot be satisfied).
--
--  (Because of lazy evaluation, this should be as efficient as
--  applying the filter as the search proceeds.  I started to build
--  the filter logic into the query function itself, with consequent
--  increase in complexity, until I remembered lazy evaluation lets
--  me keep things separate.)
--
rdfQueryFilter ::
    RDFVarBindingFilter -> [RDFVarBinding] -> [RDFVarBinding]
rdfQueryFilter qbf = filter (vbfTest qbf)

------------------------------------------------------------
--  Backward-chaining RDF graph queries
------------------------------------------------------------

-- |Reverse graph-query function.
--
--  Similar to 'rdfQueryFind', but with different success criteria.
--  The query graph is matched against the supplied graph,
--  but not every triple of the query is required to be matched.
--  Rather, every triple of the target graph must be matched,
--  and substitutions for just the variables thus bound are
--  returned.  In effect, these are subsitutions in the query
--  that entail the target graph (where @rdfQueryFind@ returns
--  substitutions that are entailed by the target graph).
--
--  Multiple substitutions may be used together, so the result
--  returned is a list of lists of query bindings.  Each inner
--  list contains several variable bindings that must all be applied
--  separately to the closure antecendents to obtain a collection of
--  expressions that together are antecedent to the supplied
--  conclusion.  A null list of bindings returned means the
--  conclusion can be inferred without any antecedents.
--
--  Note:  in back-chaining, the conditions required to prove each
--  target triple are derived independently, using the inference rule
--  for each such triple, so there are no requirements to check
--  consistency with previously determined variable bindings, as
--  there are when doing forward chaining.  A result of this is that
--  there may be redundant triples generated by the back-chaining
--  process.  Any process using back-chaining should deal with the
--  results returned accordingly.
--
--  An empty outer list is returned if no combination of
--  substitutions can infer the supplied target.
--
rdfQueryBack :: RDFGraph -> RDFGraph -> [[RDFVarBinding]]
rdfQueryBack qg tg =
    rdfQueryBack1 matchQueryVariable [] (getArcs qg) (getArcs tg)

rdfQueryBack1 ::
    NodeQuery RDFLabel -> [RDFVarBinding] -> [Arc RDFLabel] -> [Arc RDFLabel]
    -> [[RDFVarBinding]]
rdfQueryBack1 _     initv _   []       = [initv]
rdfQueryBack1 nodeq initv qas (ta:tas) = concat
    [ rdfQueryBack1 nodeq (nv:initv) qas tas
    | nv <- rdfQueryBack2 nodeq qas ta ]

--  Match a query against a single graph term, and return any new sets of
--  variable bindings thus defined.  Each member of the result is an
--  alternative possible set of variable bindings.  An empty list returned
--  means no match.
--
rdfQueryBack2 ::
    NodeQuery RDFLabel -> [Arc RDFLabel] -> Arc RDFLabel
    -> [RDFVarBinding]
rdfQueryBack2 nodeq qas ta =
    [ fromJust b | qa <- qas, let b = getBinding nodeq qa ta, isJust b ]

-- |RDF back-chaining query filter.  This function applies a supplied
--  query binding filter to the result from a call of 'rdfQueryBack'.
--
--  Each inner list contains bindings that must all be used to satisfy
--  the backchain query, so if any query binding does not satisfy the
--  filter, the entire corresponding row is removed
rdfQueryBackFilter ::
    RDFVarBindingFilter -> [[RDFVarBinding]] -> [[RDFVarBinding]]
rdfQueryBackFilter qbf = filter (all (vbfTest qbf))

-- |RDF back-chaining query modifier.  This function applies a supplied
--  query binding modifier to the result from a call of 'rdfQueryBack'.
--
--  Each inner list contains bindings that must all be used to satisfy
--  a backchaining query, so if any query binding does not satisfy the
--  filter, the entire corresponding row is removed
--
rdfQueryBackModify ::
    VarBindingModify a b -> [[VarBinding a b]] -> [[VarBinding a b]]
rdfQueryBackModify qbm = concatMap (rdfQueryBackModify1 qbm)

--  Auxiliary back-chaining query variable binding modifier function:
--  for a supplied list of variable bindings, all of which must be used
--  together when backchaining:
--  (a) make each list member into a singleton list
--  (b) apply the binding modifier to each such list, which may result
--      in a list with zero, one or more elements.
--  (c) return the listProduct of these, each member of which is
--      an alternative list of variable bindings, where the members of
--      each alternative must be used together.
--
rdfQueryBackModify1 ::
    VarBindingModify a b -> [VarBinding a b] -> [[VarBinding a b]]
rdfQueryBackModify1 qbm qbs = listProduct $ map (vbmApply qbm . (:[])) qbs

------------------------------------------------------------
--  Simple entailment graph query
------------------------------------------------------------

-- |Simple entailment (instance) graph query.
--
--  This function queries a graph to find instances of the
--  query graph in the target graph.  It is very similar
--  to the normal forward chaining query 'rdfQueryFind',
--  except that blank nodes rather than query variable nodes
--  in the query graph are matched against nodes in the target
--  graph.  Neither graph should contain query variables.
--
--  An instance is defined by the RDF semantics specification,
--  per <http://www.w3.org/TR/rdf-mt/>, and is obtained by replacing
--  blank nodes with URIs, literals or other blank nodes.  RDF
--  simple entailment can be determined in terms of instances.
--  This function looks for a subgraph of the target graph that
--  is an instance of the query graph, which is a necessary and
--  sufficient condition for RDF entailment (see the Interpolation
--  Lemma in RDF Semantics, section 1.2).
--
--  It is anticipated that this query function can be used in
--  conjunction with backward chaining to determine when the
--  search for sufficient antecendents to determine some goal
--  has been concluded.
rdfQueryInstance :: RDFGraph -> RDFGraph -> [RDFVarBinding]
rdfQueryInstance =
    rdfQueryPrim1 matchQueryBnode nullRDFVarBinding . getArcs

------------------------------------------------------------
--  Primitive RDF graph query support functions
------------------------------------------------------------

-- |Type of query node testing function.  Return value is:
--
--  * @Nothing@    if no match
--
--  * @Just True@  if match with new variable binding
--
--  * @Just False@ if match with new variable binding
--
type NodeQuery a = a -> a -> Maybe Bool

--  Extract query binding from matching a single query triple with a
--  target triple, returning:
--  - Nothing if the query is not matched
--  - Just nullVarBinding if there are no new variable bindings
--  - Just binding is a new query binding for this match
getBinding ::
    NodeQuery RDFLabel -> Arc RDFLabel -> Arc RDFLabel
    -> Maybe RDFVarBinding
getBinding nodeq (Arc s1 p1 o1) (Arc s2 p2 o2) =
    makeBinding [(s1,s2),(p1,p2),(o1,o2)] []
    where
        makeBinding [] bs = Just $ makeVarBinding bs
        makeBinding (vr@(v,r):bvrs) bs =
            case nodeq v r of
                Nothing    -> Nothing
                Just False -> makeBinding bvrs bs
                Just True  -> makeBinding bvrs (vr:bs)

--  Match variable node against target node, returning
--  Nothing if they do not match, Just True if a variable
--  node is matched (thereby creating a new variable binding)
--  or Just False if a non-blank node is matched.
matchQueryVariable :: NodeQuery RDFLabel
matchQueryVariable (Var _) _ = Just True
matchQueryVariable q t
    | q == t    = Just False
    | otherwise = Nothing

--  Match blank query node against target node, returning
--  Nothing if they do not match, Just True if a blank node
--  is matched (thereby creating a new equivalence) or
--  Just False if a non-blank node is matched.
matchQueryBnode :: NodeQuery RDFLabel
matchQueryBnode (Blank _) _ = Just True
matchQueryBnode q t
    | q == t    = Just False
    | otherwise = Nothing

------------------------------------------------------------
--  Substitute results from RDF query back into a graph
------------------------------------------------------------

-- |Graph substitution function.
--
--  Uses the supplied variable bindings to substitute variables in
--  a supplied graph, returning a list of result graphs corresponding
--  to each set of variable bindings applied to the input graph.
--  This function is used for formward chaining substitutions, and
--  returns only those result graphs for which all query variables
--  are bound.
rdfQuerySubs :: [RDFVarBinding] -> RDFGraph -> [RDFGraph]
rdfQuerySubs vars gr =
    map fst $ filter (null . snd) $ rdfQuerySubsAll vars gr

-- |Graph back-substitution function.
--
--  Uses the supplied variable bindings from 'rdfQueryBack' to perform
--  a series of variable substitutions in a supplied graph, returning
--  a list of lists of result graphs corresponding to each set of variable
--  bindings applied to the input graphs.
--
--  The outer list of the result contains alternative antecedent lists
--  that satisfy the query goal.  Each inner list contains graphs that
--  must all be inferred to satisfy the query goal.
rdfQueryBackSubs ::
    [[RDFVarBinding]] -> RDFGraph -> [[(RDFGraph,[RDFLabel])]]
rdfQueryBackSubs varss gr = [ rdfQuerySubsAll v gr | v <- varss ]

-- |Graph substitution function.
--
--  This function performs the substitutions and returns a list of
--  result graphs each paired with a list unbound variables in each.
rdfQuerySubsAll :: [RDFVarBinding] -> RDFGraph -> [(RDFGraph,[RDFLabel])]
rdfQuerySubsAll vars gr = [ rdfQuerySubs2 v gr | v <- vars ]

-- |Graph substitution function.
--
--  This function performs each of the substitutions in 'vars', and
--  replaces any nodes corresponding to unbound query variables
--  with new blank nodes.
rdfQuerySubsBlank :: [RDFVarBinding] -> RDFGraph -> [RDFGraph]
rdfQuerySubsBlank vars gr =
    [ remapLabels vs bs makeBlank g
    | v <- vars
    , let (g,vs) = rdfQuerySubs2 v gr
    , let bs     = allLabels isBlank g
    ]

-- |Graph back-substitution function, replacing variables with bnodes.
--
--  Uses the supplied variable bindings from 'rdfQueryBack' to perform
--  a series of variable substitutions in a supplied graph, returning
--  a list of lists of result graphs corresponding to each set of variable
--  bindings applied to the input graphs.
--
--  The outer list of the result contains alternative antecedent lists
--  that satisfy the query goal.  Each inner list contains graphs that
--  must all be inferred to satisfy the query goal.
rdfQueryBackSubsBlank :: [[RDFVarBinding]] -> RDFGraph -> [[RDFGraph]]
rdfQueryBackSubsBlank varss gr = [ rdfQuerySubsBlank v gr | v <- varss ]

-- |This function applies a substitution for a single set of variable
--  bindings, returning the result and a list of unbound variables.
--  It uses a state transformer monad to collect the list of
--  unbound variables.
--
--  Adding an empty graph forces elimination of duplicate arcs.
rdfQuerySubs2 :: RDFVarBinding -> RDFGraph -> (RDFGraph,[RDFLabel])
rdfQuerySubs2 varb gr = (add emptyRDFGraph g, S.toList vs)
    where
        (g,vs) = runState ( Traversable.traverse (mapNode varb) gr ) S.empty

--  Auxiliary monad function for rdfQuerySubs2.
--  This returns a state transformer Monad which in turn returns the
--  substituted node value based on the supplied query variable bindings.
--  The monad state is a set of labels which accumulates all those
--  variables seen for which no substitution was available.
mapNode :: RDFVarBinding -> RDFLabel -> State (S.Set RDFLabel) RDFLabel
mapNode varb lab =
    case vbMap varb lab of
        Just v  -> return v
        Nothing -> when (isQueryVar lab) (modify (S.insert lab)) >> return lab

------------------------------------------------------------
--  Simple lightweight query primitives
------------------------------------------------------------
--
--  [[[TODO:  modify above code to use these for all graph queries]]]

-- |Take a predicate on an
--  RDF statement and a graph, and returns all statements in the graph
--  satisfying that predicate.
--
--  Use combinations of these as follows:
--
--  * find all statements with given subject:
--          @rdfFindArcs (rdfSubjEq s)@
--
--  * find all statements with given property:
--          @rdfFindArcs (rdfPredEq p)@
--
--  * find all statements with given object:
--          @rdfFindArcs (rdfObjEq  o)@
--
--  * find all statements matching conjunction of these conditions:
--          @rdfFindArcs ('allp' [...])@
--
--  * find all statements matching disjunction of these conditions:
--          @rdfFindArcs ('anyp' [...])@
--
--  Custom predicates can also be used.
--
rdfFindArcs :: (RDFTriple -> Bool) -> RDFGraph -> [RDFTriple]
rdfFindArcs p = filter p . getArcs

-- |Test if statement has given subject
rdfSubjEq :: RDFLabel -> RDFTriple -> Bool
rdfSubjEq s = (s==) . arcSubj

-- |Test if statement has given predicate
rdfPredEq :: RDFLabel -> RDFTriple -> Bool
rdfPredEq p = (p==) . arcPred

-- |Test if statement has given object
rdfObjEq  :: RDFLabel -> RDFTriple -> Bool
rdfObjEq o  = (o==) . arcObj

{-
-- |Find statements with given subject
rdfFindSubj :: RDFLabel -> RDFGraph -> [RDFTriple]
rdfFindSubj s = rdfFindArcs (rdfSubjEq s)

-- |Find statements with given predicate
rdfFindPred :: RDFLabel -> RDFGraph -> [RDFTriple]
rdfFindPred p = rdfFindArcs (rdfPredEq p)
-}

-- |Find values of given predicate for a given subject
rdfFindPredVal :: 
  RDFLabel    -- ^ subject
  -> RDFLabel -- ^ predicate
  -> RDFGraph 
  -> [RDFLabel]
rdfFindPredVal s p = map arcObj . rdfFindArcs (allp [rdfSubjEq s,rdfPredEq p])

-- |Find integer values of a given predicate for a given subject
rdfFindPredInt :: 
  RDFLabel     -- ^ subject
  -> RDFLabel  -- ^ predicate
  -> RDFGraph -> [Integer]
rdfFindPredInt s p = mapMaybe getint . filter isint . pvs
    where
        pvs = rdfFindPredVal s p
        isint  = anyp
            [ isDatatyped xsdInteger
            , isDatatyped xsdNonNegInteger
            ]
        getint = mapL2V mapXsdInteger . getLiteralText

-- |Find all subjects that match (subject, predicate, object) in the graph.
rdfFindValSubj :: 
  RDFLabel     -- ^ predicate
  -> RDFLabel  -- ^ object
  -> RDFGraph 
  -> [RDFLabel]
rdfFindValSubj p o = map arcSubj . rdfFindArcs (allp [rdfPredEq p,rdfObjEq o])

------------------------------------------------------------
--  List query
------------------------------------------------------------

-- |Return a list of nodes that comprise an rdf:collection value,
--  given the head element of the collection.  If the list is
--  ill-formed then an arbitrary value is returned.
--
rdfFindList :: RDFGraph -> RDFLabel -> [RDFLabel]
rdfFindList gr hd = findhead $ rdfFindList gr findrest
    where
        findhead  = headOr (const []) $
                    map (:) (rdfFindPredVal hd resRdfFirst gr)
        findrest  = headOr resRdfNil (rdfFindPredVal hd resRdfRest gr)
        {-
        findhead  = headOr (const [])
                    [ (ob:) | Arc _ sb ob <- subgr, sb == resRdfFirst ]
        findrest  = headOr resRdfNil
                    [ ob | Arc _ sb ob <- subgr, sb == resRdfRest  ]
        subgr     = filter ((==) hd . arcSubj) $ getArcs gr
        -}
        headOr    = foldr const
        -- headOr _ (x:_) = x
        -- headOr x []    = x

------------------------------------------------------------
--  Interactive tests
------------------------------------------------------------

{-
s1 = Blank "s1"
p1 = Blank "p1"
o1 = Blank "o1"
s2 = Blank "s2"
p2 = Blank "p2"
o2 = Blank "o2"
qs1 = Var "s1"
qp1 = Var "p1"
qo1 = Var "o1"
qs2 = Var "s2"
qp2 = Var "p2"
qo2 = Var "o2"

qa1 = Arc qs1 qp1 qo1
qa2 = Arc qs2 qp2 qo2
qa3 = Arc qs2  p2 qo2
ta1 = Arc s1 p1 o1
ta2 = Arc s2 p2 o2

g1  = toRDFGraph [ta1,ta2]
g2  = toRDFGraph [qa3]

gb1  = getBinding matchQueryVariable qa1 ta1    -- ?s1=_:s1, ?p1=_:p1, ?o1=_:o1
gvs1 = qbMap (fromJust gb1) qs1                 -- _:s1
gvp1 = qbMap (fromJust gb1) qp1                 -- _:p1
gvo1 = qbMap (fromJust gb1) qo1                 -- _:o1
gvs2 = qbMap (fromJust gb1) qs2                 -- Nothing

gb3  = getBinding matchQueryVariable qa3 ta1    -- Nothing
gb4  = getBinding matchQueryVariable qa3 ta2    -- ?s2=_:s1, ?o2=_:o1

mqvs1 = matchQueryVariable qs2 s1
mqvp1 = matchQueryVariable p2  p1

--  rdfQueryFind

qfa  = rdfQueryFind g2 g1

qp2a = rdfQueryPrim2 matchQueryVariable qa3 g1
-}

{- more tests

qb1a = rdfQueryBack1 [] [qa1] [ta1,ta2]
qb1 = rdfQueryBack1 [] [qa1,qa2] [ta1,ta2]
ql1 = length qb1
qv1 = map (qb1!!0!!0) [qs1,qp1,qo1,qs2,qp2,qo2]
qv2 = map (qb1!!0!!1) [qs1,qp1,qo1,qs2,qp2,qo2]
qv3 = map (qb1!!1!!0) [qs1,qp1,qo1,qs2,qp2,qo2]
qv4 = map (qb1!!1!!1) [qs1,qp1,qo1,qs2,qp2,qo2]
qv5 = map (qb1!!2!!0) [qs1,qp1,qo1,qs2,qp2,qo2]
qv6 = map (qb1!!2!!1) [qs1,qp1,qo1,qs2,qp2,qo2]
qv7 = map (qb1!!3!!0) [qs1,qp1,qo1,qs2,qp2,qo2]
qv8 = map (qb1!!3!!1) [qs1,qp1,qo1,qs2,qp2,qo2]

qb2 = rdfQueryBack2 matchQueryVariable [qa1,qa2] ta1
ql2 = length qb2
qv1 = map (qbMap $ head qb2)        [qs1,qp1,qo1,qs2,qp2,qo2]
qv2 = map (qbMap $ head $ tail qb2) [qs1,qp1,qo1,qs2,qp2,qo2]
qb3 = rdfQueryBack2 matchQueryVariable [qa1,qa3] ta1

-}


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
