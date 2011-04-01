--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  SwishScript
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module implements the Swish script processor:  it parses a script
--  from a supplied string, and returns a list of Swish state transformer
--  functions whose effect, when applied to a state value, is to implement
--  the supplied script.
--
--  The script syntax is based loosely on Notation3, and the script parser is an
--  extension of the Notation3 parser in the module "Swish.RDF.N3Parser".
--
--------------------------------------------------------------------------------

module Swish.RDF.SwishScript
    ( parseScriptFromString )
where

import Swish.RDF.SwishMonad
    ( SwishStateIO, SwishStatus(..) 
    , modGraphs, findGraph, findFormula
    , modRules, findRule
    , modRulesets, findRuleset
    , findOpenVarModify, findDatatype
    , setInfo, setError, setStatus
    , NamedGraph(..)
    )

import Swish.RDF.RDFDatatype
    ( RDFDatatype )

import Swish.RDF.RDFRuleset
    ( RDFFormula, RDFRule
    , RDFRuleset
    , makeRDFClosureRule
    )

import Swish.RDF.RDFProof
    ( RDFProofStep, makeRDFProof, makeRDFProofStep )

import Swish.RDF.RDFVarBinding
    ( RDFVarBindingModify
    )

import Swish.RDF.RDFGraphShowM()

import Swish.RDF.RDFGraph
    ( RDFGraph, RDFLabel(..)
    , emptyRDFGraph
    , NamespaceMap
    , setNamespaces
    , merge, add
    )

import Swish.RDF.N3Parser
    ( parseAnyfromString
    , parseN3      
    , N3Parser, N3State(..)
    , whiteSpace, symbol, eof, identLetter
    , getPrefix
    , subgraph
    , n3symbol -- was uriRef2,
    , quickVariable -- was varid
    , lexUriRef
    , newBlankNode
    )

import Swish.RDF.N3Formatter
    ( formatGraphAsShowS )

import Swish.RDF.Datatype
    ( typeMkRules )

import Swish.RDF.Proof
    ( explainProof, showsProof )

import Swish.RDF.Ruleset
    ( makeRuleset, getRulesetRule, getMaybeContextRule )

import Swish.RDF.Rule
    ( Formula(..), Rule(..) -- , RuleMap
    )

import Swish.RDF.VarBinding
    ( composeSequence )

import Swish.Utils.Namespace
    ( ScopedName(..) )

import Swish.Utils.QName (QName, qnameFromURI)

import Swish.Utils.LookupMap
    ( mapReplaceOrAdd )

import Swish.Utils.ListHelpers
    ( equiv, flist )

import Swish.Utils.ErrorM
    ( ErrorM(..), errorToEither )

import Text.ParserCombinators.Parsec
    ( (<?>), (<|>)
    , many, manyTill, option, sepBy, between, try, notFollowedBy
    , string, char, anyChar
    , getState
    )

import Control.Monad.State
    ( modify, gets, lift
    -- , StateT(..), execStateT
    )

import Control.Monad (unless, when, liftM)

import qualified System.IO.Error as IO

------------------------------------------------------------
--  Parser for Swish script processor
------------------------------------------------------------
--
--  The parser is based on the Notation3 parser, and uses many
--  of the same syntax productions, but the top-level productions used
--  are quite different.
--
-- NOTE: during the parser re-write we strip out some of this functionality
-- 
parseScriptFromString :: Maybe QName -> String -> ErrorM [SwishStateIO ()]
parseScriptFromString base inp =
    case parseAnyfromString script base inp of
        Left  err -> Error  err
        Right scs -> Result scs

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

script :: N3Parser [SwishStateIO ()]
script = do
  whiteSpace
  scs <- many command
  eof
  return scs

isymbol :: String -> N3Parser ()
isymbol s = symbol s >> return ()

command :: N3Parser (SwishStateIO ())
command =
        do  { try $ isymbol "@prefix"
            ; getPrefix
            ; return $ return ()
            }
    <|> nameItem
    <|> readGraph
    <|> writeGraph
    <|> mergeGraphs
    <|> compareGraphs
    <|> assertEquiv
    <|> assertMember
    <|> defineRule
    <|> defineRuleset
    <|> defineConstraints
    <|> checkProofCmd
    <|> fwdChain
    <|> bwdChain
    <?>
        "script command"

nameItem :: N3Parser (SwishStateIO ())
nameItem =
        --  name :- graph
        --  name :- ( graph* )
        do  { u <- n3symbol
            ; isymbol ":-"
            ; g <- graphOrList
            ; return $ ssAddGraph u g
            }

readGraph :: N3Parser (SwishStateIO ())
readGraph =
        --  @read name  [ <uri> ]
        do  { commandName "@read"
            ; n <- n3symbol
            ; u <- option "" lexUriRef
            ; return $ ssRead n (if null u then Nothing else Just u)
            }

writeGraph :: N3Parser (SwishStateIO ())
writeGraph =
        --  @write name [ <uri> ] ; Comment
        do  { commandName "@write"
            ; n <- n3symbol
            ; let gs = ssGetList n :: SwishStateIO (Either String [RDFGraph])
            ; u <- option "" lexUriRef
            ; isymbol ";"
            ; c <- restOfLine
            ; let muri = if null u then Nothing else Just u
            ; return $ ssWriteList muri gs c
            }

mergeGraphs :: N3Parser (SwishStateIO ())
mergeGraphs =
        --  @merge ( name* ) => name
        do  { commandName "@merge"
            ; gs <- graphList
            ; isymbol "=>"
            ; n <- n3symbol
            ; return $ ssMerge n gs
            }

compareGraphs :: N3Parser (SwishStateIO ())
compareGraphs =
        --  @compare  name name
        do  { commandName "@compare"
            ; n1 <- n3symbol
            ; n2 <- n3symbol
            ; return $ ssCompare n1 n2
            }

assertEquiv :: N3Parser (SwishStateIO ())
assertEquiv =
        --  @asserteq name name ; Comment
        do  { commandName "@asserteq"
            ; n1 <- n3symbol
            ; n2 <- n3symbol
            ; isymbol ";"
            ; c <- restOfLine
            ; return $ ssAssertEq n1 n2 c
            }

assertMember :: N3Parser (SwishStateIO ())
assertMember =
        --  @assertin name name ; Comment
        do  { commandName "@assertin"
            ; n1 <- n3symbol
            ; n2 <- n3symbol
            ; isymbol ";"
            ; c <- restOfLine
            ; return $ ssAssertIn n1 n2 c
            }

defineRule :: N3Parser (SwishStateIO ())
defineRule =
        --  @rule name :- ( name* ) => name [ | ( (name var*)* ) ]
        do  { commandName "@rule"
            ; rn <- n3symbol
            ; isymbol ":-"
            ; ags <- graphOrList
            ; isymbol "=>"
            ; cg  <- graphExpr
            ; vms <- option [] varModifiers
            ; return $ ssDefineRule rn ags cg vms
            }

defineRuleset :: N3Parser (SwishStateIO ())
defineRuleset =
        --  @ruleset name :- ( name* ) ; ( name* )
        do  { commandName "@ruleset"
            ; sn <- n3symbol
            ; isymbol ":-"
            ; ags <- nameList
            ; isymbol ";"
            ; rns <- nameList
            ; return $ ssDefineRuleset sn ags rns
            }

defineConstraints :: N3Parser (SwishStateIO ())
defineConstraints =
        --  @constraints pref :- ( name* ) | ( name* )
        do  { commandName "@constraints"
            ; sn <- n3symbol
            ; isymbol ":-"
            ; cgs <- graphOrList
            ; isymbol "|"
            ; cns <- nameOrList
            ; return $ ssDefineConstraints sn cgs cns
            }

checkProofCmd :: N3Parser (SwishStateIO ())
checkProofCmd =
        --  @proof name ( name* )
        --    @input name
        --    @step name ( name* ) => name  # rule-name, antecedents, consequent
        --    @result name
        do  { commandName "@proof"
            ; pn  <- n3symbol
            ; sns <- nameList
            ; commandName "@input"
            ; igf <- formulaExpr
            ; sts <- many checkStep
            ; commandName "@result"
            ; rgf <- formulaExpr
            ; return $ ssCheckProof pn sns igf sts rgf
            }

checkStep ::
    N3Parser (Either String [RDFRuleset]
                -> SwishStateIO (Either String RDFProofStep))
checkStep =
        do  { commandName "@step"
            ; rn   <- n3symbol
            ; agfs <- formulaList
            ; isymbol "=>"
            ; cgf  <- formulaExpr
            ; return $ ssCheckStep rn agfs cgf
            }

fwdChain :: N3Parser (SwishStateIO ())
fwdChain =
        --  #   ruleset rule (antecedents) => result
        --  @fwdchain pref name ( name* ) => name
        do  { commandName "@fwdchain"
            ; sn  <- n3symbol
            ; rn  <- n3symbol
            ; ags <- graphOrList
            ; isymbol "=>"
            ; cn  <- n3symbol
            ; s <- getState             :: N3Parser N3State
            ; let prefs = prefixUris s  :: NamespaceMap
            ; return $ ssFwdChain sn rn ags cn prefs
            }

bwdChain :: N3Parser (SwishStateIO ())
bwdChain =
        --  #   ruleset rule consequent <= (antecedent-alts)
        --  @bwdchain pref name graph <= name
        do  { commandName "@bwdchain"
            ; sn  <- n3symbol
            ; rn  <- n3symbol
            ; cg  <- graphExpr
            ; isymbol "<="
            ; an  <- n3symbol
            ; s <- getState             :: N3Parser N3State
            ; let prefs = prefixUris s  :: NamespaceMap
            ; return $ ssBwdChain sn rn cg an prefs
            }

----------------------------------------------------------------------
--  Syntax clause helpers
----------------------------------------------------------------------

commandName :: String -> N3Parser ()
commandName cmd = try $
        do  { _ <- string cmd
            ; notFollowedBy identLetter
            ; whiteSpace
            }

restOfLine :: N3Parser String
restOfLine =
        do  { s <- manyTill anyChar (char '\n')
            ; whiteSpace
            ; return s
            }

nameList :: N3Parser [ScopedName]
nameList =
        do  { isymbol "("
            ; ns <- many n3symbol
            ; isymbol ")"
            ; return ns
            }

nameOrList :: N3Parser [ScopedName]
nameOrList =
        do  { n <- n3symbol
            ; return [n]
            }
    <|>
        nameList
    <?>
        "Name, or list of names"

graphExpr :: N3Parser (SwishStateIO (Either String RDFGraph))
graphExpr =
        graphOnly
    <|>
        do  { f <- formulaExpr
            ; return $ liftM (liftM formExpr) f
            }
    <?>
        "Graph expression, graph name or named graph definition"

graphOnly :: N3Parser (SwishStateIO (Either String RDFGraph))
graphOnly =
        do  { isymbol "{"
            ; b <- newBlankNode
            ; g <- subgraph b       :: N3Parser RDFGraph
            ; isymbol "}"
            ; s <- getState
            ; let gp = setNamespaces (prefixUris s) g
            ; return $ return (Right gp)
            }

graphList :: N3Parser [SwishStateIO (Either String RDFGraph)]
graphList = between (symbol "(") (symbol ")") (many graphExpr)
    <?>
        "List of graphs"

graphOrList :: N3Parser [SwishStateIO (Either String RDFGraph)]
graphOrList =
        do  { g <- graphExpr
            ; return [g]
            }
    <|>
        graphList
    <?>
        "Graph, or list of graphs"

formulaExpr :: N3Parser (SwishStateIO (Either String RDFFormula))
formulaExpr =
        do  { n <- n3symbol
            ; namedGraph n
            }
    <?> "Formula (name or named graph)"

namedGraph :: ScopedName -> N3Parser (SwishStateIO (Either String RDFFormula))
namedGraph n =
        do  { isymbol ":-"
            ; g <- graphOnly
            ; return $ ssAddReturnFormula n g
            }
    <|>
        return (ssGetFormula n)

formulaList :: N3Parser [SwishStateIO (Either String RDFFormula)]
formulaList = between (symbol "(") (symbol ")") (many formulaExpr)
    <?>
        "List of formulae (names or named graphs)"

varModifiers :: N3Parser [(ScopedName,[RDFLabel])]
varModifiers =
        do  { isymbol "|"
            ; varModList
            }

varModList :: N3Parser [(ScopedName,[RDFLabel])]
varModList =
        do  { isymbol "("
            ; vms <- sepBy varMod (symbol ",")
            ; isymbol ")"
            ; return vms
            }
    <|>
        do  { vm <- varMod
            ; return [vm]
            }

varMod :: N3Parser (ScopedName,[RDFLabel])
varMod =
        do  { rn  <- n3symbol
            ; vns <- many quickVariable
            ; return (rn,vns)
            }

----------------------------------------------------------------------
--  SwishState helper functions
----------------------------------------------------------------------
--
--  The functions below operate in the SwishStateIO monad, and are used
--  to assemble an executable version of the parsed script.

ssAddReturnFormula ::
    ScopedName -> SwishStateIO (Either String RDFGraph)
    -> SwishStateIO (Either String RDFFormula)
ssAddReturnFormula nam gf =
        do  { egr <- gf
            ; ssAddGraph nam [return egr]
            ; return $ liftM (Formula nam) egr
            }

ssAddGraph ::
    ScopedName -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO ()
ssAddGraph nam gf =
    let errmsg = "Graph/list not added: "++show nam++"; "
    in
        do  { esg <- sequence gf        -- [Either String RDFGraph]
            ; let egs = sequence esg    -- Either String [RDFGraph]
            ; let fgs = case egs of
                    Left  er -> setError  (errmsg++er)
                    Right gs -> modGraphs (mapReplaceOrAdd (NamedGraph nam gs))
            ; modify fgs
            }

ssGetGraph :: ScopedName -> SwishStateIO (Either String RDFGraph)
ssGetGraph nam =
        do  { grs <- ssGetList nam
            ; return $ liftM head grs
            }

ssGetFormula :: ScopedName -> SwishStateIO (Either String RDFFormula)
ssGetFormula nam = gets find
    where
        find st = case findFormula nam st of
            Nothing -> Left ("Formula not present: "++show nam)
            Just gr -> Right gr

ssGetList :: ScopedName -> SwishStateIO (Either String [RDFGraph])
ssGetList nam = gets find
    where
        find st = case findGraph nam st of
            Nothing  -> Left ("Graph or list not present: "++show nam)
            Just grs -> Right grs

ssRead :: ScopedName -> Maybe String -> SwishStateIO ()
ssRead nam muri = ssAddGraph nam [ssReadGraph muri]

ssReadGraph :: Maybe String -> SwishStateIO (Either String RDFGraph)
ssReadGraph muri = 
  let gf inp = case inp of
        Left  es -> Left es
        Right is -> errorToEither (parseN3 is (fmap qnameFromURI muri))
        
  in gf `liftM` getResourceData muri

ssWriteList ::
    Maybe String -> SwishStateIO (Either String [RDFGraph]) -> String
    -> SwishStateIO ()
ssWriteList muri gf comment =
        do  { esgs <- gf
            ; case esgs of
                Left  er   -> modify $ setError ("Cannot write list: "++er)
                Right [gr] -> ssWriteGraph muri gr comment
                Right grs  -> sequence_ writegrs where
                    writegrs = if null grs
                        then [putResourceData Nothing ("+ Swish: Writing empty list"++)]
                        else map writegr (zip [(0::Int)..] grs)
                    writegr (n,gr) = ssWriteGraph (murin muri n) gr
                        ("["++show n++"] "++comment)
                    murin Nothing    _ = Nothing
                    murin (Just uri) n = Just (inituri++show n++lasturi)
                        where
                            splituri1 = splitBy (=='/') uri
                            splituri2 = splitBy (=='.') (lastseg splituri1)
                            inituri   = concat (initseg splituri1 ++ initseg splituri2)
                            lasturi   = lastseg splituri2
            }

splitBy :: (a->Bool) -> [a] -> [[a]]
splitBy _ []  = []
splitBy p (s0:str) = let (s1,sr) = break p str in
    (s0:s1):splitBy p sr

lastseg :: [[a]] -> [a]
lastseg []   = []
lastseg [_]  = []
lastseg ass  = last ass

initseg :: [[a]] -> [[a]]
initseg []   = []
initseg [as] = [as]
initseg ass  = init ass

{-
ssWrite ::
    Maybe String -> SwishStateIO (Either String RDFGraph) -> String
    -> SwishStateIO ()
ssWrite muri gf comment =
        do  { esg <- gf
            ; case esg of
                Left  er -> modify $ setError ("Cannot write graph: "++er)
                Right gr -> ssWriteGraph muri gr comment
            }
-}

ssWriteGraph :: Maybe String -> RDFGraph -> String -> SwishStateIO ()
ssWriteGraph muri gr comment =
    putResourceData muri ((c++) . formatGraphAsShowS gr)
    where
        c = "# "++comment++"\n"

ssMerge ::
    ScopedName -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO ()
ssMerge nam gfs =
    let errmsg = "Graph merge not defined: "++show nam++"; "
    in
        do  { esg <- sequence gfs       -- [Either String RDFGraph]
            ; let egs = sequence esg    -- Either String [RDFGraph]
            ; let fgs = case egs of
                    Left  er -> setError  (errmsg++er)
                    Right [] -> setError  (errmsg++"No graphs to merge")
                    Right gs -> modGraphs (mapReplaceOrAdd (NamedGraph nam [g]))
                            where g = foldl1 merge gs
            ; modify fgs
            }

ssCompare :: ScopedName -> ScopedName -> SwishStateIO ()
ssCompare n1 n2 =
        do  { g1 <- ssGetGraph n1
            ; g2 <- ssGetGraph n2
            ; when (g1 /= g2) (modify $ setStatus SwishGraphCompareError)
            }

ssAssertEq :: ScopedName -> ScopedName -> String -> SwishStateIO ()
ssAssertEq n1 n2 comment =
    let er1 = ":\n  Graph or list compare not performed:  invalid graph/list."
    in
        do  { g1 <- ssGetList n1
            ; g2 <- ssGetList n2
            ; case (g1,g2) of
                (Left er,_) -> modify $ setError (comment++er1++"\n  "++er)
                (_,Left er) -> modify $ setError (comment++er1++"\n  "++er)
                (Right gr1,Right gr2) ->
                    unless (equiv gr1 gr2) $ modify $
                      setError (comment++":\n  Graph "++show n1
                                ++" differs from "++show n2++".")
            }

ssAssertIn :: ScopedName -> ScopedName -> String -> SwishStateIO ()
ssAssertIn n1 n2 comment =
    let er1 = ":\n  Membership test not performed:  invalid graph."
        er2 = ":\n  Membership test not performed:  invalid list."
    in
        do  { g1 <- ssGetGraph n1
            ; g2 <- ssGetList  n2
            ; case (g1,g2) of
                (Left er,_) -> modify $ setError (comment++er1++"\n  "++er)
                (_,Left er) -> modify $ setError (comment++er2++"\n  "++er)
                (Right gr,Right gs) ->
                    unless (gr `elem` gs) $ modify $
                    setError (comment++":\n  Graph "++show n1
                              ++" not a member of "++show n2)
            }

--  Note:  this is probably incomplete, though it should work in simple cases.
--  A complete solution would have the binding modifiers subject to
--  re-arrangement to suit the actual bound variables encountered.
--  See VarBinding.findCompositions and VarBinding.findComposition
--
--  This code should be adequate if variable bindings are always used
--  in combinations consisting of a single modifier followed by any number
--  of filters.
--
ssDefineRule ::
    ScopedName
    -> [SwishStateIO (Either String RDFGraph)]
    -> SwishStateIO (Either String RDFGraph)
    -> [(ScopedName,[RDFLabel])]
    -> SwishStateIO ()
ssDefineRule rn agfs cgf vmds =
    let errmsg1 = "Rule definition error in antecedent graph(s): "
        errmsg2 = "Rule definition error in consequent graph: "
        errmsg3 = "Rule definition error in variable modifier(s): "
        errmsg4 = "Incompatible variable binding modifier sequence"
    in
        do  { aesg <- sequence agfs     -- [Either String RDFGraph]
            ; let ags = sequence aesg   :: Either String [RDFGraph]
            ; cg <- cgf                 -- Either String RDFGraph
            ; let vmfs = map ssFindVarModify vmds
            ; evms <- sequence vmfs     -- [Either String RDFVarBindingModify]
            ; let vms = sequence evms   :: Either String [RDFVarBindingModify]
            ; let frl = case (ags,cg,vms) of
                    (Left er,_,_) -> setError (errmsg1++er)
                    (_,Left er,_) -> setError (errmsg2++er)
                    (_,_,Left er) -> setError (errmsg3++er)
                    (Right agrs,Right cgr,Right vbms) ->
                        let
                            newRule = makeRDFClosureRule rn agrs cgr
                        in
                        case composeSequence vbms of
                            Just vm -> modRules (mapReplaceOrAdd (newRule vm))
                            Nothing -> setError errmsg4
            ; modify frl
            }

ssFindVarModify ::
    (ScopedName,[RDFLabel]) -> SwishStateIO (Either String RDFVarBindingModify)
ssFindVarModify (nam,lbs) = gets $ \st ->
  case findOpenVarModify nam st of
    Just ovbm -> Right (ovbm lbs)
    Nothing   -> Left  ("Undefined modifier: "++show nam)

ssDefineRuleset ::
    ScopedName
    -> [ScopedName]
    -> [ScopedName]
    -> SwishStateIO ()
ssDefineRuleset sn ans rns =
    let errmsg1 = "Error in ruleset axiom(s): "
        errmsg2 = "Error in ruleset rule(s): "
    in
        do  { let agfs = mapM ssGetFormula ans
                                        :: SwishStateIO [Either String RDFFormula]
            ; aesg <- agfs              -- [Either String RDFFormula]
            ; let eags = sequence aesg  :: Either String [RDFFormula]
            ; let erlf = mapM ssFindRule rns
                                        :: SwishStateIO [Either String RDFRule]
            ; rles <- erlf              -- [Either String RDFRule]
            ; let erls = sequence rles  :: Either String [RDFRule]
            ; let frs = case (eags,erls) of
                    (Left er,_) -> setError (errmsg1++er)
                    (_,Left er) -> setError (errmsg2++er)
                    (Right ags,Right rls) ->
                        modRulesets (mapReplaceOrAdd rs)
                        where
                            rs = makeRuleset (snScope sn) ags rls
            ; modify frs
            }

ssFindRule :: ScopedName -> SwishStateIO (Either String RDFRule)
ssFindRule nam = gets find
    where
        find st = case findRule nam st of
            Nothing -> Left ("Rule not found: "++show nam)
            Just rl -> Right rl

ssDefineConstraints  ::
    ScopedName
    -> [SwishStateIO (Either String RDFGraph)]
    -> [ScopedName]
    -> SwishStateIO ()
ssDefineConstraints  sn cgfs dtns =
    let errmsg1 = "Error in constraint graph(s): "
        errmsg2 = "Error in datatype(s): "
    in
        do  { cges <- sequence cgfs     -- [Either String RDFGraph]
            ; let ecgs = sequence cges  :: Either String [RDFGraph]
            ; let ecgr = case ecgs of
                    Left er   -> Left er
                    Right []  -> Right emptyRDFGraph
                    Right grs -> Right $ foldl1 merge grs
            ; edtf <- mapM ssFindDatatype dtns
                                        -- [Either String RDFDatatype]
            ; let edts = sequence edtf   :: Either String [RDFDatatype]
            ; let frs = case (ecgr,edts) of
                    (Left er,_) -> setError (errmsg1++er)
                    (_,Left er) -> setError (errmsg2++er)
                    (Right cgr,Right dts) ->
                        modRulesets (mapReplaceOrAdd rs)
                        where
                            rs  = makeRuleset (snScope sn) [] rls
                            rls = concatMap (`typeMkRules` cgr) dts
            ; modify frs
            }

ssFindDatatype :: ScopedName -> SwishStateIO (Either String RDFDatatype)
ssFindDatatype nam = gets find
    where
        find st = case findDatatype nam st of
            Nothing -> Left ("Datatype not found: "++show nam)
            Just dt -> Right dt


ssCheckProof ::
    ScopedName                                      -- proof name
    -> [ScopedName]                                 -- ruleset names
    -> SwishStateIO (Either String RDFFormula)      -- input formula
    -> [Either String [RDFRuleset]                  -- proof step from rulesets
        -> SwishStateIO (Either String RDFProofStep)]
    -> SwishStateIO (Either String RDFFormula)      -- result formula
    -> SwishStateIO ()
ssCheckProof pn sns igf stfs rgf =
    let
        infmsg1 = "Proof satisfied: "
        errmsg1 = "Error in proof ruleset(s): "
        errmsg2 = "Error in proof input: "
        errmsg3 = "Error in proof step(s): "
        errmsg4 = "Error in proof goal: "
        errmsg5 = "Proof not satisfied: "
        proofname = " (Proof "++show pn++")"
    in
        do  { let rs1 = map ssFindRuleset sns       :: [SwishStateIO (Either String RDFRuleset)]
            ; rs2 <- sequence rs1                   -- [Either String RDFRuleset]
            ; let erss = sequence rs2               :: Either String [RDFRuleset]
            ; eig <- igf                            -- Either String RDFFormula
            ; let st1  = sequence $ flist stfs erss :: SwishStateIO [Either String RDFProofStep]
            ; st2 <- st1                            -- [Either String RDFProofStep]
            ; let ests = sequence st2               :: Either String [RDFProofStep]
            ; erg  <- rgf                           -- Either String RDFFormula
            ; let proof = case (erss,eig,ests,erg) of
                    (Left er,_,_,_) -> Left (errmsg1++er++proofname)
                    (_,Left er,_,_) -> Left (errmsg2++er++proofname)
                    (_,_,Left er,_) -> Left (errmsg3++er++proofname)
                    (_,_,_,Left er) -> Left (errmsg4++er++proofname)
                    (Right rss, Right ig, Right sts, Right rg) ->
                        Right (makeRDFProof rss ig rg sts)
            ; when False $ case proof of
                    (Left  _)  -> return ()
                    (Right pr) -> putResourceData Nothing $
                                    (("Proof "++show pn++"\n")++)
                                    . showsProof "\n" pr
            ; let checkproof = case proof of
                    (Left  er) -> setError er
                    (Right pr) ->
                        case explainProof pr of
                            Nothing -> setInfo (infmsg1++show pn)
                            Just ex -> setError (errmsg5++show pn++", "++ex)
                        {-
                        if not $ checkProof pr then
                            setError (errmsg5++show pn)
                        else
                            setInfo (infmsg1++show pn)
                        -}
            ; modify checkproof
            }

ssCheckStep ::
    ScopedName                                      -- rule name
    -> [SwishStateIO (Either String RDFFormula)]    -- antecedent graph formulae
    -> SwishStateIO (Either String RDFFormula)      -- consequent graph formula
    -> Either String [RDFRuleset]                   -- rulesets
    -> SwishStateIO (Either String RDFProofStep)    -- resulting proof step
ssCheckStep _  _    _   (Left  er)  = return $ Left er
ssCheckStep rn eagf ecgf (Right rss) =
    let
        errmsg1 = "Rule not in proof step ruleset(s): "
        errmsg2 = "Error in proof step antecedent graph(s): "
        errmsg3 = "Error in proof step consequent graph: "
    in
        do  { let mrul = getMaybeContextRule rn rss :: Maybe RDFRule
            ; esag <- sequence eagf                 -- [Either String RDFFormula]]
            ; let eags = sequence esag              :: Either String [RDFFormula]
            ; ecg  <- ecgf                          -- Either String RDFFormula
            ; let est = case (mrul,eags,ecg) of
                    (Nothing,_,_) -> Left (errmsg1++show rn)
                    (_,Left er,_) -> Left (errmsg2++er)
                    (_,_,Left er) -> Left (errmsg3++er)
                    (Just rul,Right ags,Right cg) ->
                        Right $ makeRDFProofStep rul ags cg
            ; return est
            }

ssFwdChain ::
    ScopedName                                      -- ruleset name
    -> ScopedName                                   -- rule name
    -> [SwishStateIO (Either String RDFGraph)]      -- antecedent graphs
    -> ScopedName                                   -- consequent graph name
    -> NamespaceMap                                 -- prefixes for new graph
    -> SwishStateIO ()
ssFwdChain sn rn agfs cn prefs =
    let
        errmsg1 = "FwdChain rule error: "
        errmsg2 = "FwdChain antecedent error: "
    in
        do  { erl  <- ssFindRulesetRule sn rn
            ; aesg <- sequence agfs     -- [Either String RDFGraph]
            ; let eags = sequence aesg   :: Either String [RDFGraph]
            ; let fcr = case (erl,eags) of
                    (Left er,_) -> setError (errmsg1++er)
                    (_,Left er) -> setError (errmsg2++er)
                    (Right rl,Right ags) ->
                        modGraphs (mapReplaceOrAdd (NamedGraph cn [cg]))
                        where
                            cg = case fwdApply rl ags of
                                []  -> emptyRDFGraph
                                grs -> setNamespaces prefs $ foldl1 add grs
            ; modify fcr
            }

ssFindRulesetRule ::
    ScopedName -> ScopedName -> SwishStateIO (Either String RDFRule)
ssFindRulesetRule sn rn = gets find
    where
        find st = case findRuleset sn st of
            Nothing -> Left ("Ruleset not found: "++show sn)
            Just rs -> find1 rs
        find1 rs = case getRulesetRule rn rs of
            Nothing -> Left ("Rule not in ruleset: "++show sn++": "++show rn)
            Just rl -> Right rl

ssFindRuleset ::
    ScopedName -> SwishStateIO (Either String RDFRuleset)
ssFindRuleset sn = gets find
    where
        find st = case findRuleset sn st of
            Nothing -> Left ("Ruleset not found: "++show sn)
            Just rs -> Right rs

ssBwdChain ::
    ScopedName                                      -- ruleset name
    -> ScopedName                                   -- rule name
    -> SwishStateIO (Either String RDFGraph)        -- consequent graphs
    -> ScopedName                                   -- antecedent alts name
    -> NamespaceMap                                 -- prefixes for new graphs
    -> SwishStateIO ()
ssBwdChain sn rn cgf an prefs =
    let
        errmsg1 = "BwdChain rule error: "
        errmsg2 = "BwdChain goal error: "
    in
        do  { erl <- ssFindRulesetRule sn rn
            ; ecg <- cgf                -- Either String RDFGraph
            ; let fcr = case (erl,ecg) of
                    (Left er,_) -> setError (errmsg1++er)
                    (_,Left er) -> setError (errmsg2++er)
                    (Right rl,Right cg) ->
                        modGraphs (mapReplaceOrAdd (NamedGraph an ags))
                        where
                            ags  = map mergegr (bwdApply rl cg)
                            mergegr grs = case grs of
                                [] -> emptyRDFGraph
                                _  -> setNamespaces prefs $ foldl1 add grs
            ; modify fcr
            }

--  Temporary implementation:  just read local file WNH     
--  (Add logic to separate filenames from URIs, and
--  attempt HTTP GET, or similar.)
getResourceData :: Maybe String -> SwishStateIO (Either String String)
getResourceData muri =
    case muri of
        Nothing  -> fromStdin
        Just uri -> fromUri uri
    where
    fromStdin =
        do  { dat <- lift getContents
            ; return $ Right dat
            }
    fromUri = fromFile
    fromFile uri =
        do  { dat <- lift $ readFile uri
            ; return $ Right dat
            }

--  Temporary implementation:  just write local file
--  (Need to add logic to separate filenames from URIs, and
--  attempt HTTP PUT, or similar.)
putResourceData :: Maybe String -> ShowS -> SwishStateIO ()
putResourceData muri gsh =
    do  { ios <- lift $ IO.try $
            case muri of
                Nothing  -> toStdout
                Just uri -> toUri uri
        ; case ios of
            Left  ioe -> modify $ setError
                            ("Error writing graph: "++
                             IO.ioeGetErrorString ioe)
            Right a   -> return a
        }
    where
        toStdout  = putStrLn gstr
        toUri uri = writeFile uri gstr
        gstr = gsh ""

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
