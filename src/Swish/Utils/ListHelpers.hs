--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ListHelpers
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module defines some generic list and related helper functions. The plan
--  is to move to using functionality from other modules (e.g. containers) where
--  possible.
--
--------------------------------------------------------------------------------

module Swish.Utils.ListHelpers
       ( -- list of modules the routine is used in
         equiv -- GraphMatch, RDF.Ruleset, Script, VarBinding, Data.LookupMap
               -- tests: GraphPartitionTest, RDFGraphTest, TestHelpers
       , flist -- Datatype, RDF.Proof, RDF.Ruleset, Script, VarBinding, ...
        
      )
where

------------------------------------------------------------
--  Set functions
--
--  NOTE: to change to Data.Set then Eq a constraint will 
--        likely need changing to Ord a
------------------------------------------------------------

-- |Subset test

subset          :: (Eq a) => [a] -> [a] -> Bool
a `subset` b    = and [ ma `elem` b | ma <- a ]

-- |Set equivalence test

equiv           :: (Eq a) => [a] -> [a] -> Bool
a `equiv` b     = a `subset` b && b `subset` a

------------------------------------------------------------
--  Functions, lists and monads
------------------------------------------------------------

-- |Apply list of functions to some value, returning list of results.
--  It's kind of like an converse map.
--
--  This is similar to the 'ap' function in the Monad library.
--
flist :: [a->b] -> a -> [b]
flist fs a = map ($ a) fs

{-
flisttest = flist [(1*),(2*),(3*)] 5 -- [5,10,15]
-}

{-

-- |A more generalized form of flist that works with arbitrary Monads.
--  (Suggested by Derek Elkin.)

fmonad :: Monad m => m (a->b) -> a -> m b
fmonad fm a =
    do  { f <- fm
        ; return $ f a
        }

-}

{-
fmonadtest = fmonad [(1*),(2*),(3*)] 3 -- [3,6,9]
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
