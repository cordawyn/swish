--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RDFDatatypeXsdInteger
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines the structures used by swish to represent and
--  manipulate RDF datatypes.
--
--------------------------------------------------------------------------------

module Swish.RDF.RDFDatatypeXsdInteger
    ( rdfDatatypeXsdInteger
    , rdfDatatypeValXsdInteger
    , typeNameXsdInteger, namespaceXsdInteger
    , axiomsXsdInteger, rulesXsdInteger
    , prefixXsdInteger
    )
where

import Swish.RDF.RDFRuleset
    ( RDFFormula, RDFRule, RDFRuleset 
    , makeRDFGraphFromN3String
    , makeRDFFormula
    )

import Swish.RDF.RDFDatatype
    ( RDFDatatype
    , RDFDatatypeVal
    , RDFDatatypeMod
    , makeRdfDtOpenVarBindingModifiers
    )

import Swish.RDF.ClassRestrictionRule
    ( makeRDFDatatypeRestrictionRules
    )

import Swish.RDF.MapXsdInteger
    ( mapXsdInteger
    )

import Swish.RDF.Datatype
    ( Datatype(..)
    , DatatypeVal(..)
    , DatatypeRel(..), DatatypeRelPr
    , altArgs
    , UnaryFnTable,    unaryFnApp
    , BinaryFnTable,   binaryFnApp
    , BinMaybeFnTable, binMaybeFnApp
    , DatatypeMod(..) 
    , makeVmod_1_1_inv, makeVmod_1_1
    , makeVmod_2_1_inv, makeVmod_2_1
    , makeVmod_2_0
    , makeVmod_2_2
    )

import Swish.RDF.Ruleset (makeRuleset)

import Swish.Utils.Namespace
    ( Namespace(..)
    , ScopedName(..)
    )

import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceXSD
    , namespaceXsdType
    )

import Data.Maybe (maybeToList)

import Control.Monad (liftM)

------------------------------------------------------------
--  Misc values
------------------------------------------------------------

--  Local name for Integer datatype
nameXsdInteger :: String
nameXsdInteger      = "integer"

-- |Type name for xsd:integer datatype
typeNameXsdInteger :: ScopedName
typeNameXsdInteger  = ScopedName namespaceXSD nameXsdInteger

-- |Namespace for xsd:integer datatype functions
namespaceXsdInteger :: Namespace
namespaceXsdInteger = namespaceXsdType nameXsdInteger

--  Helper to catenate strings with newline separator,
--  used for making textual representations of graphs.
--  (the newline makes N3 parser diagnostics easier to interpret)
--
infixr 5 +++
(+++) :: String -> ShowS
(+++) str = ((str++"\n")++)

--  Compose with function of two arguments
c2 :: (b -> c) -> (a -> d -> b) -> a -> d -> c
c2 = (.) . (.)

--  Integer power (exponentiation) function
--  returns Nothing if exponent is negative.
--
intPower :: Integer -> Integer -> Maybe Integer
intPower a b = if b < 0 then Nothing else Just (intPower1 a b)
    where
        intPower1 x y
            | q == 1           = atopsq*x
            | p == 0           = 1
            | otherwise        = atopsq
            where
                (p,q)  = y `divMod` 2
                atop   = intPower1 x p
                atopsq = atop*atop

------------------------------------------------------------
--  Declare exported RDFDatatype value for xsd:integer
------------------------------------------------------------

rdfDatatypeXsdInteger :: RDFDatatype
rdfDatatypeXsdInteger = Datatype rdfDatatypeValXsdInteger

------------------------------------------------------------
--  Implmentation of RDFDatatypeVal for xsd:integer
------------------------------------------------------------

-- |Define Datatype value for xsd:integer
--  Members of this datatype are positive or negative integer values.
--
--  The lexical form consists of an option @+@ or @-@
--  followed by a sequence of decimal digits.
--
--  The canonical lexical form has leading zeros and @+@ sign removed.
--
rdfDatatypeValXsdInteger :: RDFDatatypeVal Integer
rdfDatatypeValXsdInteger = DatatypeVal
    { tvalName      = typeNameXsdInteger
    , tvalRules     = rdfRulesetXsdInteger  -- Ruleset RDFGraph
    , tvalMkRules   = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdInteger
                                            -- RDFGraph -> [RDFRules]
    , tvalMkMods    = makeRdfDtOpenVarBindingModifiers rdfDatatypeValXsdInteger
    , tvalMap       = mapXsdInteger         -- DatatypeMap Integer
    , tvalRel       = relXsdInteger         -- [DatatypeRel Integer]
    , tvalMod       = modXsdInteger         -- [DatatypeMod Integer]
    }

-- |relXsdInteger contains arithmetic and other relations for xsd:Integer values.
--
--  The functions are inspired by those defined by CWM as math: properties.
--  (cf. http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html)
--

relXsdInteger :: [DatatypeRel Integer]
relXsdInteger =
    [ relXsdIntegerAbs
    , relXsdIntegerNeg
    , relXsdIntegerSum
    , relXsdIntegerDiff
    , relXsdIntegerProd
    , relXsdIntegerDivMod
    , relXsdIntegerPower
    , relXsdIntegerEq
    , relXsdIntegerNe
    , relXsdIntegerLt
    , relXsdIntegerLe
    , relXsdIntegerGt
    , relXsdIntegerGe
    ]

mkIntRel2 ::
    String -> DatatypeRelPr Integer -> UnaryFnTable Integer
    -> DatatypeRel Integer
mkIntRel2 nam pr fns = DatatypeRel
    { dtRelName = ScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns unaryFnApp
    }

mkIntRel3 ::
    String -> DatatypeRelPr Integer -> BinaryFnTable Integer
    -> DatatypeRel Integer
mkIntRel3 nam pr fns = DatatypeRel
    { dtRelName = ScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns binaryFnApp
    }

mkIntRel3maybe ::
    String -> DatatypeRelPr Integer -> BinMaybeFnTable Integer
    -> DatatypeRel Integer
mkIntRel3maybe nam pr fns = DatatypeRel
    { dtRelName = ScopedName namespaceXsdInteger nam
    , dtRelFunc = altArgs pr fns binMaybeFnApp
    }

relXsdIntegerAbs :: DatatypeRel Integer
relXsdIntegerAbs = mkIntRel2 "abs" (const True)
    [ ( (>=0),      [ (abs,1) ] )
    , ( const True, [ (id,0), (negate,0) ] )
    ]

relXsdIntegerNeg :: DatatypeRel Integer
relXsdIntegerNeg = mkIntRel2 "neg" (const True)
    [ ( const True, [ (negate,1) ] )
    , ( const True, [ (negate,0) ] )
    ]

relXsdIntegerSum :: DatatypeRel Integer
relXsdIntegerSum = mkIntRel3 "sum" (const True)
    [ ( const True, [ ((+),1,2) ] )
    , ( const True, [ ((-),0,2) ] )
    , ( const True, [ ((-),0,1) ] )
    ]

relXsdIntegerDiff :: DatatypeRel Integer
relXsdIntegerDiff = mkIntRel3 "diff" (const True)
    [ ( const True, [ ((-),1,2) ] )
    , ( const True, [ ((+),0,2) ] )
    , ( const True, [ ((-),1,0) ] )
    ]

relXsdIntegerProd :: DatatypeRel Integer
relXsdIntegerProd = mkIntRel3 "prod" (const True)
    [ ( const True, [ ((*),1,2) ] )
    , ( const True, [ (div,0,2) ] )
    , ( const True, [ (div,0,1) ] )
    ]

relXsdIntegerDivMod :: DatatypeRel Integer
relXsdIntegerDivMod = mkIntRel3 "divmod" (const True)
    [ ( const True, [ (div,2,3) ] )
    , ( const True, [ (mod,2,3) ] )
    , ( const True, [ ] )
    , ( const True, [ ] )
    ]

relXsdIntegerPower :: DatatypeRel Integer
relXsdIntegerPower = mkIntRel3maybe "power" (const True)
    [ ( const True, [ (liftM (:[]) `c2` intPower,1,2) ] )
    , ( const True, [ ] )
    , ( (>=0),      [ ] )
    ]

liftL2 :: (a->a->Bool) -> ([a]->a) -> ([a]->a) -> [a] -> Bool
liftL2 p i1 i2 as = p (i1 as) (i2 as)

lcomp :: (a->a->Bool) -> [a] -> Bool
lcomp p = liftL2 p head (head . tail)

-- eq

relXsdIntegerEq :: DatatypeRel Integer
relXsdIntegerEq = mkIntRel2 "eq" (lcomp (==))
    ( repeat (const True, []) )

-- ne

relXsdIntegerNe :: DatatypeRel Integer
relXsdIntegerNe = mkIntRel2 "ne" (lcomp (/=))
    ( repeat (const True, []) )

-- lt

relXsdIntegerLt :: DatatypeRel Integer
relXsdIntegerLt = mkIntRel2 "lt" (lcomp (<))
    ( repeat (const True, []) )

-- le

relXsdIntegerLe :: DatatypeRel Integer
relXsdIntegerLe = mkIntRel2 "le" (lcomp (<=))
    ( repeat (const True, []) )

-- gt

relXsdIntegerGt :: DatatypeRel Integer
relXsdIntegerGt = mkIntRel2 "gt" (lcomp (>))
    ( repeat (const True, []) )

-- ge

relXsdIntegerGe :: DatatypeRel Integer
relXsdIntegerGe = mkIntRel2 "ge" (lcomp (>=))
    ( repeat (const True, []) )

-- |modXsdInteger contains variable binding modifiers for xsd:Integer values.
--
--  The functions are selected from those defined by CWM as math:
--  properties.
--  (cf. http://www.w3.org/2000/10/swap/doc/CwmBuiltins.html)
--
modXsdInteger :: [RDFDatatypeMod Integer]
modXsdInteger =
    [ modXsdIntegerAbs
    , modXsdIntegerNeg
    , modXsdIntegerSum
    , modXsdIntegerDiff
    , modXsdIntegerProd
    , modXsdIntegerDivMod
    , modXsdIntegerPower
    , modXsdIntegerEq
    , modXsdIntegerNe
    , modXsdIntegerLt
    , modXsdIntegerLe
    , modXsdIntegerGt
    , modXsdIntegerGe
    ]

modXsdIntegerAbs :: RDFDatatypeMod Integer
modXsdIntegerAbs = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "abs"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod_1_1
    }
    where
        f0 vs@[v1,v2] = if v1 == abs v2 then vs else []
        f0 _          = []
        f1 [v2]       = [abs v2]
        f1 _          = []

modXsdIntegerNeg :: RDFDatatypeMod Integer
modXsdIntegerNeg = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "neg"
    , dmModf = [ f0, f1, f1 ]
    , dmAppf = makeVmod_1_1_inv
    }
    where
        f0 vs@[v1,v2] = if v1 == negate v2 then vs else []
        f0 _          = []
        f1 [vi]       = [-vi]
        f1 _          = []

modXsdIntegerSum :: RDFDatatypeMod Integer
modXsdIntegerSum = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "sum"
    , dmModf = [ f0, f1, f2, f2 ]
    , dmAppf = makeVmod_2_1_inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2+v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2+v3]
        f1 _             = []
        f2 [v1,vi]       = [v1-vi]
        f2 _             = []

modXsdIntegerDiff :: RDFDatatypeMod Integer
modXsdIntegerDiff = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "diff"
    , dmModf = [ f0, f1, f2, f3 ]
    , dmAppf = makeVmod_2_1_inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2-v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2-v3]
        f1 _             = []
        f2 [v1,v3]       = [v1+v3]
        f2 _             = []
        f3 [v1,v2]       = [v2-v1]
        f3 _             = []

modXsdIntegerProd :: RDFDatatypeMod Integer
modXsdIntegerProd = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "prod"
    , dmModf = [ f0, f1, f2, f2 ]
    , dmAppf = makeVmod_2_1_inv
    }
    where
        f0 vs@[v1,v2,v3] = if v1 == v2*v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = [v2*v3]
        f1 _             = []
        f2 [v1,vi]       = if r == 0 then [q] else []
            where (q,r)  = quotRem v1 vi
        f2 _             = []

modXsdIntegerDivMod :: RDFDatatypeMod Integer
modXsdIntegerDivMod = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "divmod"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod_2_2
    }
    where
        f0 vs@[v1,v2,v3,v4] = if (v1,v2) == divMod v3 v4 then vs else []
        f0 _                = []
        f1 [v3,v4]          = [v1,v2] where (v1,v2) = divMod v3 v4
        f1 _                = []

modXsdIntegerPower :: RDFDatatypeMod Integer
modXsdIntegerPower = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger "power"
    , dmModf = [ f0, f1 ]
    , dmAppf = makeVmod_2_1
    }
    where
        f0 vs@[v1,v2,v3] = if Just v1 == intPower v2 v3 then vs else []
        f0 _             = []
        f1 [v2,v3]       = maybeToList (intPower v2 v3)
        f1 _             = []

modXsdIntegerEq, modXsdIntegerNe, modXsdIntegerLt, modXsdIntegerLe, modXsdIntegerGt, modXsdIntegerGe :: RDFDatatypeMod Integer 
modXsdIntegerEq = modXsdIntegerCompare "eq" (==)
modXsdIntegerNe = modXsdIntegerCompare "ne" (/=)
modXsdIntegerLt = modXsdIntegerCompare "lt" (<)
modXsdIntegerLe = modXsdIntegerCompare "le" (<=)
modXsdIntegerGt = modXsdIntegerCompare "gt" (>)
modXsdIntegerGe = modXsdIntegerCompare "ge" (>=)

modXsdIntegerCompare ::
    String -> (Integer->Integer->Bool) -> RDFDatatypeMod Integer
modXsdIntegerCompare nam rel = DatatypeMod
    { dmName = ScopedName namespaceXsdInteger nam
    , dmModf = [ f0 ]
    , dmAppf = makeVmod_2_0
    }
    where
        f0 vs@[v1,v2] = if rel v1 v2 then vs else []
        f0 _          = []

-- |rulesetXsdInteger contains rules and axioms that allow additional
--  deductions when xsd:integer values appear in a graph.
--
--  The rules defined here are concerned with basic integer arithmetic
--  operations: +, -, *, div, rem
--
--  makeRuleset :: Namespace -> [Formula ex] -> [Rule ex] -> Ruleset ex
--
rdfRulesetXsdInteger :: RDFRuleset
rdfRulesetXsdInteger =
    makeRuleset namespaceXsdInteger axiomsXsdInteger rulesXsdInteger

mkPrefix :: Namespace -> String
mkPrefix ns =
    "@prefix " ++ nsPrefix ns ++ ": <" ++ nsURI ns ++ "> . \n"

prefixXsdInteger :: String
prefixXsdInteger =
    mkPrefix namespaceRDF  ++
    mkPrefix namespaceRDFS ++
    mkPrefix namespaceRDFD ++
    mkPrefix namespaceXSD  ++
    mkPrefix namespaceXsdInteger ++
    " \n"

mkAxiom :: String -> String -> RDFFormula
mkAxiom local gr =
    makeRDFFormula namespaceXsdInteger local (prefixXsdInteger++gr)

axiomsXsdInteger :: [RDFFormula]
axiomsXsdInteger =
    [ mkAxiom "dt"      "xsd:integer rdf:type rdfs:Datatype ."
    ]

rulesXsdInteger :: [RDFRule]
rulesXsdInteger = makeRDFDatatypeRestrictionRules rdfDatatypeValXsdInteger gr
    where
        gr = makeRDFGraphFromN3String rulesXsdIntegerStr

rulesXsdIntegerStr :: String
rulesXsdIntegerStr = prefixXsdInteger
    +++ "xsd_integer:Abs a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:abs ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Neg a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:neg ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Sum a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    +++ "  rdfd:constraint xsd_integer:sum ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Diff a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    +++ "  rdfd:constraint xsd_integer:diff ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Prod a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    +++ "  rdfd:constraint xsd_integer:prod ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:DivMod a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3 rdf:_4) ; "
    +++ "  rdfd:constraint xsd_integer:divmod ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Power a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2 rdf:_3) ; "
    +++ "  rdfd:constraint xsd_integer:power ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Eq a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:eq ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Ne a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:ne ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Lt a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:lt ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Le a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:le ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Gt a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:gt ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "
    +++ "xsd_integer:Ge a rdfd:GeneralRestriction ; "
    +++ "  rdfd:onProperties (rdf:_1 rdf:_2) ; "
    +++ "  rdfd:constraint xsd_integer:ge ; "
    +++ "  rdfd:maxCardinality \"1\"^^xsd:nonNegativeInteger . "

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