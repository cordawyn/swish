--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This module is a wrapper for the main program of Swish.
--
--------------------------------------------------------------------------------

import Paths_swish (version)
import Data.Version (showVersion)

import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess,ExitFailure), exitWith)

import Control.Monad (unless)

import System.IO (stderr, hPutStrLn)

import Swish.HaskellRDF.SwishMain

------------------------------------------------------------
--  Swish main program
------------------------------------------------------------
--
--  This is a minimal wrapper for the real main program, to facilitate
--  interactive execution (e.g. in HUGS) of different command lines.
--
--  execStateT runs the monad with a supplied initial state,
--  then separates the resulting state from the IO monad.

main :: IO ()
main = do
  args <- getArgs
  unless ("-q" `elem` args) $ reportVersion
  code <- runSwishArgs args
  case code of
    SwishSuccess -> exitWith ExitSuccess
    _ -> hPutStrLn stderr ("Swish: "++show code) >> 
         exitWith (ExitFailure (fromEnum code))
  
reportVersion :: IO ()
reportVersion = putStrLn $ "Swish-" ++ showVersion version ++ " CLI\n\n" 
  
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
