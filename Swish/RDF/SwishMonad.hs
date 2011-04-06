{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  SwishMonad
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  FlexibleInstances and MultiParamTypeClasses
--
--  SwishMonad:  Composed state and IO monad for Swish
--
--------------------------------------------------------------------------------

module Swish.RDF.SwishMonad
    ( SwishStateIO, SwishState(..), SwishStatus(..)
    , setFormat, setBase, setGraph
    , modGraphs, findGraph, findFormula
    , modRules, findRule
    , modRulesets, findRuleset
    , findOpenVarModify, findDatatype
    , setInfo, resetInfo, setError, resetError
    , setStatus
    -- , setVerbose
    , emptyState
    , SwishFormat(..)
    , NamedGraph(..), NamedGraphMap
    , swishError
    , reportLines, reportLine
    )
where

import Swish.RDF.RDFGraph
    ( RDFGraph, emptyRDFGraph )

import Swish.RDF.RDFRuleset
    ( RDFFormula, RDFRule, RDFRuleMap, RDFRuleset, RDFRulesetMap )

import Swish.RDF.RDFDatatype
    ( RDFDatatype )

import Swish.RDF.RDFVarBinding
    ( RDFOpenVarBindingModify
    )

import Swish.RDF.BuiltInMap
    ( findRDFOpenVarBindingModifier
    , findRDFDatatype
    , rdfRulesetMap
    )

import Swish.RDF.Ruleset
    ( getMaybeContextAxiom
    , getMaybeContextRule
    )

import Swish.RDF.Rule
    ( Formula(..)
    )

import Swish.Utils.Namespace (ScopedName(..))
import Swish.Utils.QName (QName)

import Swish.Utils.LookupMap
    ( LookupEntryClass(..), LookupMap(..)
    , emptyLookupMap
    , mapFindMaybe
    , mapVals
    )

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (StateT(..), modify)

import System.IO (hPutStrLn, stderr)

------------------------------------------------------------
--  State and state monad for Swish program
------------------------------------------------------------
--
--  Uses StateT Monad transformer:
--  See example by Mark Carroll at http://www.haskell.org/hawiki/MonadState

{-|
The supported input and output formats.
-}
data SwishFormat = 
  N3  -- ^ N3 format
  | NT -- ^ NTriples format
    deriving Eq

instance Show SwishFormat where
  show N3  = "N3"
  show NT  = "Ntriples"
  -- show RDF = "RDF/XML"

data SwishState = SwishState
    { format    :: SwishFormat      -- ^ format to use for I/O
    , base      :: Maybe QName      -- ^ base to use rather than file name
    , graph     :: RDFGraph         -- ^ current graph
    , graphs    :: NamedGraphMap    -- ^ script processor named graphs
    , rules     :: RDFRuleMap       -- ^ script processor named rules
    , rulesets  :: RDFRulesetMap    -- ^ script processor rulesets
    , infomsg   :: Maybe String     -- ^ information message, or Nothing
    , errormsg  :: Maybe String     -- ^ error message, or Nothing
    , exitcode  :: SwishStatus      -- ^ current status message
    }

-- | Status of the processor
--
data SwishStatus =
  SwishSuccess               -- ^ successful run
  | SwishGraphCompareError   -- ^ graphs do not compare
  | SwishDataInputError      -- ^ input data problem (ie format/syntax)
  | SwishDataAccessError     -- ^ data access error
  | SwishArgumentError       -- ^ command-line argument error
  | SwishExecutionError      -- ^ error executing a Swish script
    deriving (Eq, Enum)

instance Show SwishStatus where
  show SwishSuccess           = "Success."
  show SwishGraphCompareError = "The graphs do not compare as equal."
  show SwishDataInputError    = "There was a format or syntax error in the input data."
  show SwishDataAccessError   = "There was a problem accessing data."
  show SwishArgumentError     = "Argument error: use -h or -? for help."
  show SwishExecutionError    = "There was a problem executing a Swish script."

type SwishStateIO a = StateT SwishState IO a

emptyState :: SwishState
emptyState = SwishState
    { format    = N3
    , base      = Nothing
    , graph     = emptyRDFGraph
    , graphs    = emptyLookupMap
    , rules     = emptyLookupMap
    , rulesets  = rdfRulesetMap
    , infomsg   = Nothing
    , errormsg  = Nothing
    , exitcode  = SwishSuccess
    -- , banner = True
    }

setFormat :: SwishFormat -> SwishState -> SwishState
setFormat   fm state = state { format = fm }

setBase :: Maybe QName -> SwishState -> SwishState
setBase bs state = state { base = bs }

setGraph :: RDFGraph -> SwishState -> SwishState
setGraph    gr state = state { graph = gr }

modGraphs ::
    ( NamedGraphMap -> NamedGraphMap ) -> SwishState -> SwishState
modGraphs grmod state = state { graphs = grmod (graphs state) }

findGraph :: ScopedName -> SwishState -> Maybe [RDFGraph]
findGraph nam state = mapFindMaybe nam (graphs state)

findFormula :: ScopedName -> SwishState -> Maybe RDFFormula
findFormula nam state = case findGraph nam state of
        Nothing  -> getMaybeContextAxiom nam (mapVals $ rulesets state)
        Just []  -> Just $ Formula nam emptyRDFGraph
        Just grs -> Just $ Formula nam (head grs)

modRules ::
    ( RDFRuleMap -> RDFRuleMap ) -> SwishState -> SwishState
modRules rlmod state = state { rules = rlmod (rules state) }

findRule :: ScopedName -> SwishState -> Maybe RDFRule
findRule nam state =
    let
        localrule   = mapFindMaybe nam (rules state)
        contextrule = getMaybeContextRule nam $ mapVals $ rulesets state
    in
        case localrule of
            Nothing -> contextrule
            justlr  -> justlr

modRulesets ::
    ( RDFRulesetMap -> RDFRulesetMap ) -> SwishState -> SwishState
modRulesets rsmod state = state { rulesets = rsmod (rulesets state) }

findRuleset ::
    ScopedName -> SwishState -> Maybe RDFRuleset
findRuleset nam state = mapFindMaybe (snScope nam) (rulesets state)

findOpenVarModify :: ScopedName -> SwishState -> Maybe RDFOpenVarBindingModify
findOpenVarModify nam _ = findRDFOpenVarBindingModifier nam

findDatatype :: ScopedName -> SwishState -> Maybe RDFDatatype
findDatatype nam _ = findRDFDatatype nam

setInfo :: String -> SwishState -> SwishState
setInfo msg state = state { infomsg = Just msg }

resetInfo :: SwishState -> SwishState
resetInfo state = state { infomsg = Nothing }

setError :: String -> SwishState -> SwishState
setError msg state = state { errormsg = Just msg }

resetError :: SwishState -> SwishState
resetError state = state { errormsg = Nothing }

setStatus :: SwishStatus -> SwishState -> SwishState
setStatus ec state = state { exitcode = ec }

{-
setVerbose :: Bool -> SwishState -> SwishState
setVerbose f state = state { banner = f }
-}

------------------------------------------------------------
--  Data types for Swish script dictionaries
------------------------------------------------------------
--
--  The graphs dictionary contains named graphs and/or lists
--  of graphs that are created and used by script statements.

data NamedGraph = NamedGraph
    { ngName    :: ScopedName
    , ngGraph   :: [RDFGraph]
    }

instance LookupEntryClass NamedGraph ScopedName [RDFGraph]
    where
        keyVal   (NamedGraph k v) = (k,v)
        newEntry (k,v)            = NamedGraph k v

type NamedGraphMap = LookupMap NamedGraph

------------------------------------------------------------
--  Report error and set exit status code
------------------------------------------------------------

swishError :: String -> SwishStatus -> SwishStateIO ()
swishError msg sts = do
  reportLines [msg, show sts ++ "\n"]
  -- when (sts == 4) $ reportLine "Use 'Swish -h' or 'Swish -?' for help\n"
  modify $ setStatus sts

------------------------------------------------------------
--  Output text to the standard error stream
------------------------------------------------------------
--
--  Each string in the supplied list is a line of text to
--  be displayed.

reportLines  :: [String] -> SwishStateIO ()
reportLines = mapM_ reportLine 

reportLine  :: String -> SwishStateIO ()
reportLine line =
    -- lift putStrLn line
    lift $ hPutStrLn stderr line

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
