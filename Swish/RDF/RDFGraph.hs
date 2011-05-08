{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFGraph
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances
--
--  This module defines a memory-based RDF graph instance.
--
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Simple labelled directed graph value
------------------------------------------------------------

module Swish.RDF.RDFGraph
    ( 
      -- * Labels
      RDFLabel(..), ToRDFLabel(..), FromRDFLabel(..)
    , isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral
    , isDatatyped, isMemberProp, isUri, isBlank, isQueryVar
    , getLiteralText, getScopedName, makeBlank
                                     
      -- * RDF Graphs
    , RDFTriple
    , toRDFTriple, fromRDFTriple
    , NSGraph(..)
    , RDFGraph
    , toRDFGraph, emptyRDFGraph {-, updateRDFGraph-}
    , NamespaceMap, RevNamespaceMap, RevNamespace
    , emptyNamespaceMap
    , LookupFormula(..), Formula, FormulaMap, emptyFormulaMap
    , addArc, merge
    , allLabels, allNodes, remapLabels, remapLabelList
    , newNode, newNodes
    , setNamespaces, getNamespaces
    , setFormulae, getFormulae, setFormula, getFormula
      
    -- * Re-export from GraphClass
    , LDGraph(..), Label (..), Arc(..)
    , arc, arcSubj, arcPred, arcObj, Selector
      
      -- * Export selected RDFLabel values
    , res_rdf_type, res_rdf_first, res_rdf_rest, res_rdf_nil
    , res_rdfs_member
    , res_rdfd_GeneralRestriction
    , res_rdfd_onProperties, res_rdfd_constraint, res_rdfd_maxCardinality
    , res_owl_sameAs, res_log_implies
      
      -- * Exported for testing
    , grMatchMap, grEq
    , mapnode, maplist
    )
where

import Swish.Utils.Namespace
    ( Namespace(..)
    , getScopedNameURI
    , ScopedName(..)
    , getQName
    , makeQNameScopedName
    , makeUriScopedName
    , nullScopedName
    )

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , langTag, isLang
    , rdf_type
    , rdf_first, rdf_rest, rdf_nil, rdf_XMLLiteral
    , rdfs_member
    , rdfd_GeneralRestriction
    , rdfd_onProperties, rdfd_constraint, rdfd_maxCardinality
    , owl_sameAs, log_implies
    -- , xsd_type
    , xsd_boolean, xsd_float, xsd_double, xsd_integer
    , xsd_dateTime, xsd_date                                                
    )

import Swish.RDF.GraphClass
    ( LDGraph(..), Label (..)
    , Arc(..), arc, arcSubj, arcPred, arcObj
    , Selector )

import Swish.RDF.GraphMatch (graphMatch, LabelMap, ScopedLabel(..))

import Swish.Utils.QName (QName)
import Swish.Utils.MiscHelpers (hash, quote)
import Swish.Utils.ListHelpers (addSetElem)

import Swish.Utils.LookupMap
    ( LookupMap(..), LookupEntryClass(..)
    , listLookupMap
    , mapFind, mapFindMaybe, mapReplaceOrAdd, mapVals, mapKeys )

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Control.Applicative (Applicative, liftA, (<$>), (<*>))
-- import Control.Monad (liftM, ap)

import Network.URI (URI, parseURI, uriToString)

import Data.Monoid (Monoid(..))
import Data.Char (isDigit)
import Data.List (intersect, union, findIndices)
import Data.Ord (comparing)
import Data.String (IsString(..))
import Data.Time (UTCTime, Day, ParseTime, parseTime, formatTime)
import System.Locale (defaultTimeLocale)  
import Text.Printf

-----------------------------------------------------------
-- | RDF graph node values
--
--  cf. <http://www.w3.org/TR/rdf-concepts/#section-Graph-syntax>
--
--  This is extended from the RDF abstract graph syntax in the
--  following ways:
--
--  (a) a graph can be part of a resource node or blank node
--      (cf. Notation3 formulae)
--
--  (b) a \"variable\" node option is distinguished from a
--      blank node.
--      I have found this useful for encoding and handling
--      queries, even though query variables can be expressed
--      as blank nodes.
--
--  (c) a \"NoNode\" option is defined.
--      This might otherwise be handled by @Maybe (RDFLabel g)@.
--

-- TODO: should Lit be split up so that can easily differentiate between
-- a type and a language tag

data RDFLabel =
      Res ScopedName                    -- ^ resource
    | Lit String (Maybe ScopedName)     -- ^ literal [type/language]
    | Blank String                      -- ^ blank node
    | Var String                        -- ^ variable (not used in ordinary graphs)
    | NoNode                            -- ^ no node  (not used in ordinary graphs)

instance Eq RDFLabel where
    (==) = labelEq

instance Show RDFLabel where
    show (Res sn)           = show sn
    show (Lit st Nothing)   = quote st
    show (Lit st (Just nam))
        | isLang nam = quote st ++ "@"  ++ langTag nam
        | otherwise  = quote st ++ "^^" ++ show nam
    show (Blank ln)         = "_:"++ln
    show (Var ln)           = '?' : ln
    show NoNode             = "<NoNode>"

instance Ord RDFLabel where
    -- Optimize some common cases..
    compare (Res sn1)      (Res sn2)      = compare sn1 sn2
    compare (Blank ln1)    (Blank ln2)    = compare ln1 ln2
    compare (Res _)        (Blank _)      = LT
    compare (Blank _)      (Res _)        = GT
    -- .. else use show string comparison
    compare l1 l2 = comparing show l1 l2
    
    {- <= is not used if compare is provided
    -- Similarly for <=
    (Res qn1)   <= (Res qn2)      = qn1 <= qn2
    (Blank ln1) <= (Blank ln2)    = ln1 <= ln2
    (Res _)     <= (Blank _)      = True
    (Blank _)   <= (Res _)        = False
    l1 <= l2                      = show l1 <= show l2
    -}
    
instance Label RDFLabel where
    labelIsVar (Blank _)    = True
    labelIsVar (Var _)      = True
    labelIsVar _            = False
    getLocal   (Blank loc)  = loc
    getLocal   (Var   loc)  = '?':loc
    getLocal   (Res   sn)   = "Res_"++snLocal sn
    getLocal   (NoNode)     = "None"
    getLocal   _            = "Lit_"
    makeLabel  ('?':loc)    = Var loc
    makeLabel  loc          = Blank loc
    labelHash seed lb       = hash seed (showCanon lb)

instance IsString RDFLabel where
  fromString = flip Lit Nothing

{-|
A type that can be converted to a RDF Label.

The String instance converts to an untyped literal
(so no language tag is assumed).

The `UTCTime` and `Day` instances assume values are in UTC.
 
The conversion for XSD types attempts to use the
canonical form described in section 2.3.1 of
<http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#lexical-space>.
  
-}

class ToRDFLabel a where
  toRDFLabel :: a -> RDFLabel
  
{-|
A type that can be converted from a RDF Label,
with the possibility of failure.
 
The String instance converts from an untyped literal
(so it can not be used with a string with a language tag).

The following conversions are supported for common XSD
types (out-of-band values result in @Nothing@):

 - @xsd:boolean@ to @Bool@

 - @xsd:integer@ to @Int@ and @Integer@

 - @xsd:float@ to @Float@

 - @xsd:double@ to @Double@

 - @xsd:dateTime@ to @UTCTime@

 - @xsd:date@ to @Day@

-}

class FromRDFLabel a where
  fromRDFLabel :: RDFLabel -> Maybe a

-- instances for type conversion to/from RDFLabel
  
instance ToRDFLabel RDFLabel where
  toRDFLabel = id
  
instance FromRDFLabel RDFLabel where
  fromRDFLabel = Just . id
  
-- TODO: need to check that the Haskell read/show instances match
--       the RDF syntactical constraints

maybeRead :: (Read a) => String -> Maybe a
maybeRead inStr = 
  case reads inStr of
    [(val, "")] -> Just val
    _ -> Nothing
    
fLabel :: (String -> Maybe a) -> ScopedName -> RDFLabel -> Maybe a
fLabel conv dtype (Lit xs (Just dt)) | dt == dtype = conv xs
                                     | otherwise   = Nothing
fLabel _    _     _ = Nothing
  
tLabel :: (Show a) => ScopedName -> (String -> String) -> a -> RDFLabel                      
tLabel dtype conv = flip Lit (Just dtype) . conv . show                      

instance ToRDFLabel Char where
  toRDFLabel = flip Lit Nothing . (:[])

instance FromRDFLabel Char where
  fromRDFLabel (Lit [c] Nothing) = Just c
  fromRDFLabel _ = Nothing

instance ToRDFLabel String where
  toRDFLabel = flip Lit Nothing

instance FromRDFLabel String where
  fromRDFLabel (Lit xs Nothing) = Just xs
  fromRDFLabel _ = Nothing

instance ToRDFLabel Bool where
  toRDFLabel b = Lit (if b then "true" else "false") (Just xsd_boolean)
                                                 
instance FromRDFLabel Bool where
  fromRDFLabel = fLabel conv xsd_boolean
    where
      conv s | s `elem` ["1", "true"]  = Just True
             | s `elem` ["0", "false"] = Just False
             | otherwise               = Nothing

fromRealFloat :: (RealFloat a, PrintfArg a) => ScopedName -> a -> RDFLabel
fromRealFloat dtype f | isNaN f      = toL "NaN"
                      | isInfinite f = toL $ if f > 0.0 then "INF" else "-INF"
                      | otherwise    = toL $ printf "%E" f
                        where
                          toL = flip Lit (Just dtype)

toRealFloat :: (RealFloat a, Read a) => (a -> Maybe a) -> ScopedName -> RDFLabel -> Maybe a
toRealFloat conv = fLabel rconv
    where
      rconv "NaN"  = Just (0.0/0.0) -- how best to create a NaN?
      rconv "INF"  = Just (1.0/0.0) -- ditto for Infinity
      rconv "-INF" = Just ((-1.0)/0.0)
      rconv istr 
        -- xsd semantics allows "2." but Haskell syntax does not so add on a "0" in this case
        | null istr        = Nothing
        | last istr == '.' = maybeRead (istr ++ "0") >>= conv
        | otherwise        = maybeRead istr >>= conv
      
instance ToRDFLabel Float where
  toRDFLabel = fromRealFloat xsd_float
  
instance FromRDFLabel Float where
  fromRDFLabel = toRealFloat conv xsd_float
    where
      -- assume that an invalid value (NaN/Inf) from maybeRead means
      -- that the value is out of bounds for Float so we do not
      -- convert
      conv f | isNaN f || isInfinite f = Nothing
             | otherwise               = Just f
                 
instance ToRDFLabel Double where
  toRDFLabel = fromRealFloat xsd_double
  
instance FromRDFLabel Double where
  fromRDFLabel = toRealFloat Just xsd_double
  
-- TODO: are there subtypes of xsd::integer that are  
--       useful here?  
--         
-- TODO: add in support for Int8/..., Word8/...  
--  
instance ToRDFLabel Int where
  toRDFLabel = tLabel xsd_integer id

{-
it appears that reads doesn't fail when the input is outside
the Int range; instead it overflows. So instead of

  fromRDFLabel = fLabel maybeRead xsd_integer

we convert via Integer.
-}

instance FromRDFLabel Int where
  fromRDFLabel = fLabel (\istr -> maybeRead istr >>= conv) xsd_integer
    where
      conv :: Integer -> Maybe Int
      conv i = 
        let lb = fromIntegral (minBound :: Int)
            ub = fromIntegral (maxBound :: Int)
        in if (i >= lb) && (i <= ub) then Just (fromIntegral i) else Nothing

instance ToRDFLabel Integer where
  toRDFLabel = tLabel xsd_integer id

instance FromRDFLabel Integer where
  fromRDFLabel = fLabel maybeRead xsd_integer

{-
Support an ISO-8601 style format supporting

  2005-02-28T00:00:00Z
  2004-12-31T19:01:00-05:00
  2005-07-14T03:18:56.234+01:00

fromUTCFormat is used to convert UTCTime to a string
for storage within a Lit.

toUTCFormat is used to convert a string into UTCTime;
we have to support 
   no time zone
   Z
   +/-HH:MM

which means a somewhat messy convertor, which is written
for clarity rather than speed.
-}

fromUTCFormat :: UTCTime -> String
fromUTCFormat = formatTime defaultTimeLocale "%FT%T%QZ"
  
fromDayFormat :: Day -> String
fromDayFormat = formatTime defaultTimeLocale "%FZ"
  
toTimeFormat :: (ParseTime a) => String -> String -> Maybe a
toTimeFormat fmt inVal =
  let fmtHHMM = fmt ++ "%z"
      fmtZ = fmt ++ "Z"
      pt f = parseTime defaultTimeLocale f inVal
  in case pt fmtHHMM of
    o@(Just _) -> o
    _ -> case pt fmtZ of
      o@(Just _) -> o
      _ -> pt fmt 
  
toUTCFormat :: String -> Maybe UTCTime
toUTCFormat = toTimeFormat "%FT%T%Q"
    
toDayFormat :: String -> Maybe Day
toDayFormat = toTimeFormat "%F"
    
instance ToRDFLabel UTCTime where
  toRDFLabel = flip Lit (Just xsd_dateTime) . fromUTCFormat
  
instance FromRDFLabel UTCTime where
  fromRDFLabel = fLabel toUTCFormat xsd_dateTime
  
instance ToRDFLabel Day where
  toRDFLabel = flip Lit (Just xsd_date) . fromDayFormat

instance FromRDFLabel Day where
  fromRDFLabel = fLabel toDayFormat xsd_date
  
instance ToRDFLabel ScopedName where  
  toRDFLabel = Res

instance FromRDFLabel ScopedName where
  fromRDFLabel (Res sn) = Just sn
  fromRDFLabel _        = Nothing
  
instance ToRDFLabel QName where  
  toRDFLabel = Res . makeQNameScopedName
  
instance FromRDFLabel QName where
  fromRDFLabel (Res sn) = Just $ getQName sn
  fromRDFLabel _        = Nothing
  
instance ToRDFLabel URI where  
  toRDFLabel u = Res $ makeUriScopedName $ uriToString id u ""
  
instance FromRDFLabel URI where
  fromRDFLabel (Res sn) = parseURI $ getScopedNameURI sn
  fromRDFLabel _        = Nothing

-- | Get the canonical string for RDF label.
--
--  Used for hashing, so that equivalent labels always return
--  the same hash value.
    
showCanon :: RDFLabel -> String
showCanon (Res sn)           = "<"++getScopedNameURI sn++">"
showCanon (Lit st (Just nam))
        | isLang nam = quote st ++ "@"  ++ langTag nam
        | otherwise  = quote st ++ "^^" ++ getScopedNameURI nam
showCanon s                  = show s


-- | Define equality of nodes possibly based on different graph types.
--
-- The version of equality defined here is not strictly RDF abstract syntax
-- equality, but my interpretation of equivalence for the purposes of
-- entailment, in the absence of any specific datatype knowledge other
-- than XML literals.
--
labelEq :: RDFLabel -> RDFLabel -> Bool
labelEq (Res q1)            (Res q2)        = q1 == q2
labelEq (Blank s1)          (Blank s2)      = s1 == s2
labelEq (Var v1)            (Var v2)        = v1 == v2
labelEq (Lit s1 t1)         (Lit s2 t2)     = s1 == s2 && t1 == t2
labelEq _                   _               = False

---------------------------------------------------------
--  Selected RDFLabel values
---------------------------------------------------------

res_rdf_type, res_rdf_first, res_rdf_rest, res_rdf_nil,
  res_rdfs_member, res_rdfd_GeneralRestriction,
  res_rdfd_onProperties, res_rdfd_constraint,
  res_rdfd_maxCardinality, res_owl_sameAs, res_log_implies
  :: RDFLabel

res_rdf_type                = Res rdf_type
res_rdf_first               = Res rdf_first
res_rdf_rest                = Res rdf_rest
res_rdf_nil                 = Res rdf_nil
res_rdfs_member             = Res rdfs_member
res_rdfd_GeneralRestriction = Res rdfd_GeneralRestriction
res_rdfd_onProperties       = Res rdfd_onProperties
res_rdfd_constraint         = Res rdfd_constraint
res_rdfd_maxCardinality     = Res rdfd_maxCardinality
res_owl_sameAs              = Res owl_sameAs
res_log_implies             = Res log_implies

---------------------------------------------------------
--  Additional functions on RDFLabel values
---------------------------------------------------------

-- |Test if supplied labal is a URI resource node
isUri :: RDFLabel -> Bool
isUri (Res _) = True
isUri  _      = False

-- |Test if supplied labal is a literal node
isLiteral :: RDFLabel -> Bool
isLiteral (Lit _ _) = True
isLiteral  _        = False

-- |Test if supplied labal is an untyped literal node
isUntypedLiteral :: RDFLabel -> Bool
isUntypedLiteral (Lit _ Nothing  ) = True
isUntypedLiteral (Lit _ (Just tn)) = isLang tn
isUntypedLiteral  _                = False

-- |Test if supplied labal is an untyped literal node
isTypedLiteral :: RDFLabel -> Bool
isTypedLiteral (Lit _ (Just tn)) = not (isLang tn)
isTypedLiteral  _                = False

-- |Test if supplied labal is an XML literal node
isXMLLiteral :: RDFLabel -> Bool
isXMLLiteral = isDatatyped rdf_XMLLiteral

-- |Test if supplied label is an typed literal node of a given datatype
isDatatyped :: ScopedName -> RDFLabel -> Bool
isDatatyped d  (Lit _ (Just n)) = n == d
isDatatyped _  _                = False

-- |Test if supplied label is a container membership property
--
--  Check for namespace is RDF namespace and
--  first character of local name is '_' and
--  remaining characters of local name are all digits
isMemberProp :: RDFLabel -> Bool
isMemberProp (Res sn) = snScope sn == namespaceRDF &&
                        head loc   == '_' &&
                        all isDigit (tail loc)
                        where
                            loc = snLocal sn
isMemberProp _        = False

-- |Test if supplied labal is a blank node
isBlank :: RDFLabel -> Bool
isBlank (Blank _) = True
isBlank  _        = False

-- |Test if supplied labal is a query variable
isQueryVar :: RDFLabel -> Bool
isQueryVar (Var _) = True
isQueryVar  _      = False

-- |Extract text value from a literal node
getLiteralText :: RDFLabel -> String
getLiteralText (Lit s _) = s
getLiteralText  _        = ""

-- |Extract ScopedName value from a resource node
getScopedName :: RDFLabel -> ScopedName
getScopedName (Res sn) = sn
getScopedName  _       = nullScopedName

-- |Make a blank node from a supplied query variable,
--  or return the supplied label unchanged.
--  (Use this in when substituting an existential for an
--  unsubstituted query variable.)
makeBlank :: RDFLabel -> RDFLabel
makeBlank  (Var loc)    = Blank loc
makeBlank  lb           = lb

-- | RDF Triple (statement)
-- 
--   At present there is no check or type-level
--   constraint that stops the subject or
--   predicate of the triple from being a literal.
--
type RDFTriple = Arc RDFLabel

-- | Convert 3 RDF labels to a RDF triple.
--
--   See also `arcFromTriple`.
toRDFTriple :: 
  (ToRDFLabel s, ToRDFLabel p, ToRDFLabel o) 
  => s -- ^ Subject 
  -> p -- ^ Predicate
  -> o -- ^ Object
  -> RDFTriple
toRDFTriple s p o = 
  Arc (toRDFLabel s) (toRDFLabel p) (toRDFLabel o)

-- | Extract the contents of a RDF triple.
--
--   See also `arcToTriple`.
fromRDFTriple :: 
  (FromRDFLabel s, FromRDFLabel p, FromRDFLabel o) 
  => RDFTriple 
  -> Maybe (s, p, o) -- ^ The conversion only succeeds if all three
                     --   components can be converted to the correct
                     --   Haskell types.
fromRDFTriple (Arc s p o) = 
  (,,) <$> fromRDFLabel s <*> fromRDFLabel p <*> fromRDFLabel o
  
-- | Namespace prefix list entry

type NamespaceMap = LookupMap Namespace

data RevNamespace = RevNamespace Namespace

instance LookupEntryClass RevNamespace String String where
    keyVal   (RevNamespace (Namespace pre uri)) = (uri,pre)
    newEntry (uri,pre) = RevNamespace (Namespace pre uri)

type RevNamespaceMap = LookupMap RevNamespace

emptyNamespaceMap :: NamespaceMap
emptyNamespaceMap = LookupMap []

-- | Graph formula entry

data LookupFormula lb gr = Formula
    { formLabel :: lb
    , formGraph :: gr
    }

instance (Eq lb, Eq gr) => Eq (LookupFormula lb gr) where
    f1 == f2 = formLabel f1 == formLabel f2 &&
               formGraph f1 == formGraph f2

instance (Label lb)
    => LookupEntryClass (LookupFormula lb (NSGraph lb)) lb (NSGraph lb)
    where
        keyVal fe      = (formLabel fe, formGraph fe)
        newEntry (k,v) = Formula { formLabel=k, formGraph=v }

instance (Label lb) => Show (LookupFormula lb (NSGraph lb))
    where
        show (Formula l g) = show l ++ " :- { " ++ showArcs "    " g ++ " }"

type Formula lb = LookupFormula lb (NSGraph lb)

type FormulaMap lb = LookupMap (LookupFormula lb (NSGraph lb))

emptyFormulaMap :: FormulaMap RDFLabel
emptyFormulaMap = LookupMap []

{-  given up on trying to do Functor for formulae...
instance Functor (LookupFormula (NSGraph lb)) where
    fmap f fm = mapTranslateEntries (mapFormulaEntry f) fm
-}

formulaeMap :: (lb -> l2) -> FormulaMap lb -> FormulaMap l2
formulaeMap f = fmap (formulaEntryMap f) 

formulaEntryMap ::
    (lb -> l2)
    -> Formula lb
    -> Formula l2
formulaEntryMap f (Formula k gr) = Formula (f k) (fmap f gr)

formulaeMapA :: Applicative f => (lb -> f l2) -> 
                FormulaMap lb -> f (FormulaMap l2)
formulaeMapA f = T.traverse (formulaEntryMapA f)

formulaEntryMapA ::
  (Applicative f) => 
  (lb -> f l2)
  -> Formula lb
  -> f (Formula l2)
formulaEntryMapA f (Formula k gr) = Formula `liftA` f k <*> T.traverse f gr

{-
formulaeMapM ::
    (Monad m) => (lb -> m l2) -> FormulaMap lb -> m (FormulaMap l2)
formulaeMapM f = T.mapM (formulaEntryMapM f)

formulaEntryMapM ::
    (Monad m)
    => (lb -> m l2)
    -> Formula lb
    -> m (Formula l2)
formulaEntryMapM f (Formula k gr) =
  Formula `liftM` f k `ap` T.mapM f gr
    
-}

-- | Memory-based graph with namespaces and subgraphs

data NSGraph lb = NSGraph
    { namespaces :: NamespaceMap
    , formulae   :: FormulaMap lb
    , statements :: [Arc lb]
    }

getNamespaces :: NSGraph lb -> NamespaceMap
getNamespaces = namespaces

setNamespaces      :: NamespaceMap -> NSGraph lb -> NSGraph lb
setNamespaces ns g = g { namespaces=ns }

getFormulae :: NSGraph lb -> FormulaMap lb
getFormulae = formulae

setFormulae      :: FormulaMap lb -> NSGraph lb -> NSGraph lb
setFormulae fs g = g { formulae=fs }

getFormula     :: (Label lb) => NSGraph lb -> lb -> Maybe (NSGraph lb)
getFormula g l = mapFindMaybe l (formulae g)

setFormula     :: (Label lb) => Formula lb -> NSGraph lb -> NSGraph lb
setFormula f g = g { formulae=mapReplaceOrAdd f (formulae g) }

instance (Label lb) => Monoid (NSGraph lb) where
  mempty = NSGraph emptyNamespaceMap (LookupMap []) []
  mappend = merge
  
instance (Label lb) => LDGraph NSGraph lb where
    getArcs      = statements 
    setArcs as g = g { statements=as }
    containedIn = error "containedIn for LDGraph NSGraph lb is undefined!" -- TODO: should there be one defined?

-- Optimized method to add arc .. don't check for duplicates.
addArc :: (Label lb) => Arc lb -> NSGraph lb -> NSGraph lb
addArc ar gr = gr { statements=addSetElem ar (statements gr) }

instance Functor NSGraph where
  fmap f (NSGraph ns fml stmts) =
    NSGraph ns (formulaeMap f fml) ((map $ fmap f) stmts)

instance F.Foldable NSGraph where
  foldMap = T.foldMapDefault

instance T.Traversable NSGraph where
  traverse f (NSGraph ns fml stmts) = 
    NSGraph ns <$> formulaeMapA f fml <*> (T.traverse $ T.traverse f) stmts
  
instance (Label lb) => Eq (NSGraph lb) where
    (==) = grEq

instance (Label lb) => Show (NSGraph lb) where
    show     = grShow ""
    showList = grShowList ""

grShowList :: (Label lb) => String -> [NSGraph lb] -> String -> String
grShowList _ []     = showString "[no graphs]"
grShowList p (g:gs) = showChar '[' . showString (grShow pp g) . showl gs
    where
        showl []     = showChar ']' -- showString $ "\n" ++ p ++ "]"
        showl (h:hs) = showString (",\n "++p++grShow pp h) . showl hs
        pp           = ' ':p

grShow   :: (Label lb) => String -> NSGraph lb -> String
grShow p g =
    "Graph, formulae: " ++ showForm ++ "\n" ++
    p ++ "arcs: " ++ showArcs p g
    where
        showForm = foldr ((++) . (pp ++) . show) "" fml
        fml = listLookupMap (getFormulae g)
        pp = "\n    " ++ p

showArcs :: (Label lb) => String -> NSGraph lb -> String
showArcs p g = foldr ((++) . (pp ++) . show) "" (getArcs g)
    where
        pp = "\n    " ++ p

grEq :: (Label lb) => NSGraph lb -> NSGraph lb -> Bool
grEq g1 g2 = fst ( grMatchMap g1 g2 )

grMatchMap :: (Label lb) =>
    NSGraph lb -> NSGraph lb -> (Bool, LabelMap (ScopedLabel lb))
grMatchMap g1 g2 =
    graphMatch matchable (getArcs g1) (getArcs g2)
    where
        matchable l1 l2 = mapFormula g1 l1 == mapFormula g2 l2
        mapFormula g l  = mapFindMaybe l (getFormulae g)

-- |Merge RDF graphs, renaming blank and query variable nodes as
--  needed to neep variable nodes from the two graphs distinct in
--  the resulting graph.
--        
--  Currently formulae are not preserved across a merge.
--        
merge :: (Label lb) => NSGraph lb -> NSGraph lb -> NSGraph lb
merge gr1 gr2 =
    let
        bn1   = allLabels labelIsVar gr1
        bn2   = allLabels labelIsVar gr2
        dupbn = intersect bn1 bn2
        allbn = union bn1 bn2
    in
        add gr1 (remapLabels dupbn allbn id gr2)

-- |Return list of all labels (including properties) in the graph
--  satisfying a supplied filter predicate.
allLabels :: (Label lb) => (lb -> Bool) -> NSGraph lb -> [lb]
allLabels p gr = filter p (unionNodes p (formulaNodes p gr) (labels gr) )

-- |Return list of all subjects and objects in the graph
--  satisfying a supplied filter predicate.
allNodes :: (Label lb) => (lb -> Bool) -> NSGraph lb -> [lb]
allNodes p = unionNodes p [] . nodes

-- | List all nodes in graph formulae satisfying a supplied predicate
formulaNodes :: (Label lb) => (lb -> Bool) -> NSGraph lb -> [lb]
formulaNodes p gr = foldl (unionNodes p) fkeys (map (allLabels p) fvals)
    where
        -- fm :: (Label lb) => FormulaMap lb
        --                     LookupMap LookupFormula (NSGraph lb) lb
        fm    = formulae gr
        -- fvals :: (Label lb) => [NSGraph lb]
        fvals = mapVals fm
        -- fkeys :: (Label lb) => [lb]
        fkeys = filter p $ mapKeys fm

-- | Helper to filter variable nodes and merge with those found so far
unionNodes :: (Label lb) => (lb -> Bool) -> [lb] -> [lb] -> [lb]
unionNodes p ls1 ls2 = ls1 `union` filter p ls2

-- |Remap selected nodes in graph:
--
--  This is the node renaming operation that prevents graph-scoped
--  variable nodes from being merged when two graphs are merged.
remapLabels ::
    (Label lb)
    => [lb] -- ^ variable nodes to be renamed (@dupbn@)
    -> [lb] -- ^ variable nodes used that must be avoided (@allbn@)
    -> (lb -> lb) -- ^ node conversion function that is applied to nodes
    -- from @dupbn@ in the graph that are to be replaced by
    -- new blank nodes.  If no such conversion is required,
    -- supply @id@.  The function 'makeBlank' can be used to convert
    -- RDF query nodes into RDF blank nodes.
    -> NSGraph lb -- ^ graph in which nodes are to be renamed
    -> NSGraph lb
remapLabels dupbn allbn cnvbn = fmap (mapnode dupbn allbn cnvbn)

-- |Externally callable function to construct a list of (old,new)
--  values to be used for graph label remapping.
--
remapLabelList ::
    (Label lb)
    => [lb] -- ^ labels to be remaped
    -> [lb] -- ^ labels to be avoided by the remapping
    -> [(lb,lb)]
remapLabelList remap avoid = maplist remap avoid id []

-- | Remap a single graph node.
--
--  If the node is not one of those to be remapped,
--  the supplied value is returned unchanged.
mapnode ::
    (Label lb) => [lb] -> [lb] -> (lb -> lb) -> lb -> lb
mapnode dupbn allbn cnvbn nv =
    mapFind nv nv (LookupMap (maplist dupbn allbn cnvbn []))

-- | Construct a list of (oldnode,newnode) values to be used for
--  graph label remapping.  The function operates recursively, adding
--  new nodes generated to the accumulator and also to the
--  list of nodes to be avoided.
maplist ::
    (Label lb) 
    => [lb]       -- ^ oldnode values
    -> [lb]       -- ^ nodes to be avoided
    -> (lb -> lb) 
    -> [(lb,lb)]  -- ^ accumulator
    -> [(lb,lb)]
maplist []         _     _     mapbn = mapbn
maplist (dn:dupbn) allbn cnvbn mapbn = maplist dupbn allbn' cnvbn mapbn'
    where
        dnmap  = newNode (cnvbn dn) allbn
        mapbn' = (dn,dnmap):mapbn
        allbn' = dnmap:allbn

-- |Given a node and a list of existing nodes, find a new node for
--  the supplied node that does not clash with any existing node.
--  (Generates an non-terminating list of possible replacements, and
--  picks the first one that isn't already in use.)
--
--  TODO: optimize this for common case @nnn@ and @_nnn@:
--    always generate @_nnn@ and keep track of last allocated
--
newNode :: (Label lb) => lb -> [lb] -> lb
newNode dn existnodes =
    head $ newNodes dn existnodes

-- |Given a node and a list of existing nodes, generate a list of new
--  nodes for the supplied node that do not clash with any existing node.
newNodes :: (Label lb) => lb -> [lb] -> [lb]
newNodes dn existnodes =
    filter (not . (`elem` existnodes)) $ trynodes (noderootindex dn)

noderootindex :: (Label lb) => lb -> (String,Int)
noderootindex dn = (nh,nx) where
    (nh,nt) = splitnodeid $ getLocal dn
    nx      = if null nt then 0 else read nt

splitnodeid :: String -> (String,String)
splitnodeid dn = splitAt (tx+1) dn where
    tx = last $ (-1):findIndices (not . isDigit) dn

trynodes :: (Label lb) => (String,Int) -> [lb]
trynodes (nr,nx) = [ makeLabel (nr++show n) | n <- iterate (+1) nx ]

{-
trybnodes :: (Label lb) => (String,Int) -> [lb]
trybnodes (nr,nx) = [ makeLabel (nr++show n) | n <- iterate (+1) nx ]
-}

-- | Memory-based RDF graph type

type RDFGraph = NSGraph RDFLabel

-- |Create a new RDF graph from a supplied list of arcs
toRDFGraph :: [Arc RDFLabel] -> RDFGraph
toRDFGraph arcs = emptyRDFGraph { statements = arcs }

-- |Create a new, empty RDF graph.
emptyRDFGraph :: RDFGraph
emptyRDFGraph = mempty 

{-
-- |Update an RDF graph using a supplied list of arcs, keeping
--  prefix definitions and formula definitions from the original.
--
--  [[[TODO:  I think this may be redundant - the default graph
--  class has an update method which accepts a function to update
--  the arcs, not touching other parts of the graph value.]]]
updateRDFGraph :: RDFGraph -> [Arc RDFLabel] -> RDFGraph
updateRDFGraph gr as = gr { statements=as }
-}

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
