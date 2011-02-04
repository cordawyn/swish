{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  AccummulateM
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module defines a monadic accumulator type.  The plan is that it be
--  used in conjunction with "Swish.HaskellUtils.FunctorM" and similar constructs to accumulate
--  some or all of the values visited.
--
--  Using a monad of type Accumulator, which wraps some type @c@ and is
--  also declared to be an instance of @MonadAccum Accumulator c e@, for some @e@,
--  then 'foldM' can be used to accumulate values of type @e@ with an initial
--  value of type @c@ with the instance-supplied 'growVal' method.
--
--  This module also declares accumulator instances for 'Int', 'Integer' and @[]@
--  datatypes.
--
--  This is all very well, but rather unnecessary:  it is just as easy, and
--  more standard (hence easier for other Haskell programmers to follow),
--  to use a state monad with a nullary return type; e.g.
--
--  >   execsState (stateMonadExpr) initialState
--
--  which returns the final state value.
--
--------------------------------------------------------------------------------

module Swish.HaskellUtils.AccumulateM
    ( Accumulator(..), MonadAccum(..) )
where

import Control.Monad
    ( foldM )


class (Monad m) => MonadAccum m c e | m c -> e where
    growVal :: c -> e -> m c
    reapVal :: m c -> c

data Accumulator c = Accumulator c deriving (Eq, Show)

instance Monad Accumulator where
    (Accumulator v) >>= k  = k v
    return                 = Accumulator 

instance MonadAccum Accumulator Int Int where
    growVal n m             = Accumulator (n+m)
    reapVal (Accumulator n) = n

instance MonadAccum Accumulator Integer Integer where
    growVal n m             = Accumulator (n+m)
    reapVal (Accumulator n) = n

instance MonadAccum Accumulator [v] v where
    growVal vs v             = Accumulator (v:vs)
    reapVal (Accumulator vs) = vs


--  Tests
addVal :: Int -> Int -> Accumulator Int
addVal m n = Accumulator (n+m)

testList :: [Int]
testList  = [1,2,3,4,5,6]

testList1 :: [Integer]
testList1 = [1,2,3,4,5,6]

testList2 :: String
testList2 = "plugh"

test1 :: Accumulator Int
test1 = foldM addVal 0 testList

test2 :: Accumulator Integer
test2 = Accumulator 0

test3 :: Accumulator Int
test3 = Accumulator 0 >>= addVal 1

test4 :: Accumulator Int
test4 = Accumulator 5 >>= addVal 5

test5 :: Accumulator Int
test5 = (growVal 3 :: Int -> Accumulator Int) 20

test6 :: Accumulator Int
test6 = foldM growVal 0 testList  :: Accumulator Int

test7 :: Accumulator Integer
test7 = foldM growVal 0 testList1 :: Accumulator Integer

test8 :: Int
test8 = reapVal (foldM growVal 0  testList  :: Accumulator Int)

test9 :: String
test9 = reapVal (foldM growVal [] testList2 :: Accumulator String)

test :: Bool
test = and
    [ test1 == Accumulator 21
    , test2 == Accumulator 0
    , test3 == Accumulator 1
    , test4 == Accumulator 10
    , test5 == Accumulator 23
    , test6 == Accumulator 21
    , test7 == Accumulator 21
    , test8 == 21
    , test9 == "hgulp"
    ]

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
