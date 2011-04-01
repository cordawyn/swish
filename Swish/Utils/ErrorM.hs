{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  ErrorM
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  FlexibleInstances
--
--  This module defines a Monad for reporting errors.  It is conceived as a
--  very simple extension of Maybe, except that the failure variant caries
--  a reason for failure.
--
--------------------------------------------------------------------------------

module Swish.Utils.ErrorM
    ( ErrorM(Error,Result), errorToEither )
where

import Control.Monad
    ( MonadPlus(..) )

------------------------------------------------------------
--  ErrorM
------------------------------------------------------------

-- |Error monad.
--
data ErrorM a = Error String | Result a

-- |Monad instance for Error
instance Monad ErrorM where
    (Result a) >>= f = f a
    (Error e)  >>= _ = Error e
    return     = Result
    fail       = Error

-- |MonadPlus instance for Error
instance MonadPlus ErrorM where
    mzero             = Error "No result"
    mplus (Error _) r = r
    mplus r         _ = r

-- |Convert to an Either
errorToEither :: ErrorM a -> Either String a
errorToEither (Error  e) = Left e
errorToEither (Result r) = Right r

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
