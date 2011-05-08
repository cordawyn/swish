--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  SwishCommands
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  SwishCommands:  functions to deal with indivudual Swish command options.
--
--------------------------------------------------------------------------------

module Swish.RDF.SwishCommands
    ( swishFormat
    , swishBase
    -- , swishVerbose
    , swishInput
    , swishOutput
    , swishMerge
    , swishCompare
    , swishGraphDiff
    , swishScript
    )
where

import Swish.RDF.SwishMonad
    ( SwishStateIO, SwishState(..), SwishStatus(..)
    , setFormat, setBase, setGraph
    , resetInfo, resetError, setStatus
    -- , setVerbose
    , SwishFormat(..)
    , swishError
    , reportLine
    )

import Swish.RDF.SwishScript
    ( parseScriptFromString
    )

import Swish.RDF.GraphPartition
    ( GraphPartition(..)
    , partitionGraph, comparePartitions
    , partitionShowP
    )

import Swish.RDF.RDFGraph
    ( RDFGraph, merge )

import qualified Swish.RDF.N3Formatter as N3F
import qualified Swish.RDF.NTFormatter as NTF

import Swish.RDF.N3Parser (parseN3) -- (parseN3fromString)
import Swish.RDF.NTParser (parseNT)

import Swish.RDF.GraphClass
    ( LDGraph(..)
    , Label(..)
    )

import Swish.Utils.QName (QName, qnameFromURI, qnameFromFilePath, getQNameURI)

import System.IO
    ( Handle, openFile, IOMode(..)
    , hPutStr, hPutStrLn, hClose, hGetContents
    , hIsReadable, hIsWritable
    , stdin, stdout
    )

import Network.URI (URI, 
                    relativeTo,
                    parseURI, parseURIReference, uriToString)

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (modify, gets)
import Control.Monad (liftM, when)

import System.IO.Error

import Data.Maybe (isJust, fromMaybe)

------------------------------------------------------------
--  Set file format to supplied value
------------------------------------------------------------

-- the second argument allows for options to be passed along
-- with the format (a la cwm) but this is not supported yet
--
swishFormat :: SwishFormat -> Maybe String -> SwishStateIO ()
swishFormat fmt _ = modify (setFormat fmt)

------------------------------------------------------------
--  Set base URI to supplied value
------------------------------------------------------------

-- the Maybe String argument is ignored (a result of a lack of
-- design with the command-line processing)
--
swishBase :: Maybe QName -> Maybe String -> SwishStateIO ()
swishBase mb _ = modify (setBase mb)

------------------------------------------------------------
--  Read graph from named file
------------------------------------------------------------

swishInput :: Maybe String -> SwishStateIO ()
swishInput fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . setGraph)
  
------------------------------------------------------------
--  Merge graph from named file
------------------------------------------------------------

swishMerge :: Maybe String -> SwishStateIO ()
swishMerge fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . mergeGraph)
    
mergeGraph :: RDFGraph -> SwishState -> SwishState
mergeGraph gr state = state { graph = newgr }
    where
        newgr = merge gr (graph state)

------------------------------------------------------------
--  Compare graph from named file
------------------------------------------------------------

swishCompare :: Maybe String -> SwishStateIO ()
swishCompare fnam =
  swishReadGraph fnam >>= maybe (return ()) compareGraph
    
compareGraph :: RDFGraph -> SwishStateIO ()
compareGraph gr = do
  oldGr <- gets graph
  let exitCode = if gr == oldGr then SwishSuccess else SwishGraphCompareError
  modify $ setStatus exitCode
  
------------------------------------------------------------
--  Display graph differences from named file
------------------------------------------------------------

swishGraphDiff :: Maybe String -> SwishStateIO ()
swishGraphDiff fnam =
  swishReadGraph fnam >>= maybe (return ()) diffGraph

diffGraph :: RDFGraph -> SwishStateIO ()
diffGraph gr = do
  oldGr <- gets graph
  let p1 = partitionGraph (getArcs oldGr)
      p2 = partitionGraph (getArcs gr)
      diffs = comparePartitions p1 p2
      
  swishWriteFile (swishOutputDiffs diffs) Nothing
  
swishOutputDiffs :: (Label lb) =>
    [(Maybe (GraphPartition lb),Maybe (GraphPartition lb))]
    -> Maybe String 
    -> Handle
    -> SwishStateIO ()
swishOutputDiffs diffs fnam hnd = do
  lift $ hPutStrLn hnd ("Graph differences: "++show (length diffs))
  mapM_ (swishOutputDiff fnam hnd) (zip [1..] diffs)

swishOutputDiff :: (Label lb) =>
    Maybe String 
    -> Handle
    -> (Int,(Maybe (GraphPartition lb),Maybe (GraphPartition lb)))
    -> SwishStateIO ()
swishOutputDiff fnam hnd (diffnum,(part1,part2)) = do
  lift $ hPutStrLn hnd ("---- Difference "++show diffnum++" ----")
  lift $ hPutStr hnd "Graph 1:"
  swishOutputPart fnam hnd part1
  lift $ hPutStr hnd "Graph 2:"
  swishOutputPart fnam hnd part2

swishOutputPart :: (Label lb) =>
    Maybe String 
    -> Handle 
    -> Maybe (GraphPartition lb) 
    -> SwishStateIO ()
swishOutputPart _ hnd part = 
  let out = maybe "\n(No arcs)" (partitionShowP "\n") part
  in lift $ hPutStrLn hnd out

------------------------------------------------------------
--  Execute script from named file
------------------------------------------------------------

swishScript :: Maybe String -> SwishStateIO ()
swishScript fnam = swishReadScript fnam >>= mapM_ swishCheckResult

swishReadScript :: Maybe String -> SwishStateIO [SwishStateIO ()]
swishReadScript = swishReadFile swishParseScript []

{-|
Calculate the base URI to use; it combines the file name
with any user-supplied base.

If both the file name and user-supplied base are Nothing
then the value 

   http://id.ninebynine.org/2003/Swish/

is used.

Needs some work.
-}

defURI :: QName
defURI = qnameFromURI "http://id.ninebynine.org/2003/Swish/"

calculateBaseURI ::
  Maybe FilePath -- ^ file name
  -> SwishStateIO QName -- ^ base URI
  
calculateBaseURI Nothing = fromMaybe defURI `liftM` gets base
    
calculateBaseURI (Just fnam) = do
  mbase <- gets base
  case mbase of
    Just buri -> case appendUris (getQNameURI buri) fnam of
      Left emsg -> fail emsg -- TODO: think about this ...
      Right res -> return $ qnameFromURI $ showURI res
    Nothing -> lift $ qnameFromFilePath fnam

-- this is also in N3Parser
showURI :: URI -> String
showURI u = uriToString id u ""

-- this is also in N3Parser
appendUris :: String -> String -> Either String URI
appendUris buri uri =
  case parseURI uri of
    Just absuri -> Right absuri
    _ -> case parseURIReference uri of
      Just reluri -> case parseURI buri of
        Just baseuri -> case relativeTo reluri baseuri of
          Just resuri -> Right resuri
          _ -> Left $ "Unable to append <" ++ uri ++ "> to base=<" ++ buri ++ ">"
          
        _ -> Left $ "Invalid base URI: <" ++ buri ++ ">"
      _ -> Left $ "Invalid URI: <" ++ uri ++ ">"
      
swishParseScript ::
  Maybe String -- file name (or "stdin" if Nothing)
  -> String  -- script contents
  -> SwishStateIO [SwishStateIO ()]
swishParseScript mfpath inp = do
  buri <- calculateBaseURI mfpath
  case parseScriptFromString (Just buri) inp of
    Left err -> do
      let inName = maybe "standard input" ("file " ++) mfpath
      swishError ("Script syntax error in " ++ inName ++ ": "++err) SwishDataInputError
      return []
              
    Right scs -> return scs

swishCheckResult :: SwishStateIO () -> SwishStateIO ()
swishCheckResult swishcommand = do
  swishcommand
  er <- gets errormsg
  case er of  
    Just x -> swishError x SwishExecutionError >> modify resetError
    _      -> return ()
    
  ms <- gets infomsg
  case ms of
    Just x -> reportLine x >> modify resetInfo
    _      -> return ()

------------------------------------------------------------
--  Output graph to named file
------------------------------------------------------------

swishOutput :: Maybe String -> SwishStateIO ()
swishOutput = swishWriteFile swishOutputGraph
   
swishOutputGraph :: Maybe String -> Handle -> SwishStateIO ()
swishOutputGraph _ hnd = do
  fmt <- gets format
  
  let writeOut formatter = do
        out <- gets $ formatter . graph
        lift $ hPutStrLn hnd (out "")
        
  case fmt of
    N3 -> writeOut N3F.formatGraphAsShowS
    NT -> writeOut NTF.formatGraphAsShowS
    -- _  -> swishError ("Unsupported file format: "++show fmt) SwishArgumentError

------------------------------------------------------------
--  Common input functions
------------------------------------------------------------
--
--  Keep the logic separate for reading file data and
--  parsing it to an RDF graph value.

swishReadGraph :: Maybe String -> SwishStateIO (Maybe RDFGraph)
swishReadGraph = swishReadFile swishParse Nothing

-- | Open a file (or stdin), read its contents, and process them.
--
swishReadFile :: 
  (Maybe String -> String -> SwishStateIO a) -- ^ Convert filename and contents into desired value
  -> a -- ^ the value to use if the file can not be read in
  -> Maybe String -- ^ the file name or @stdin@ if @Nothing@
  -> SwishStateIO a
swishReadFile conv errVal fnam = 
  let reader (h,f,i) = do
        res <- conv fnam i
        when f $ lift $ hClose h
        return res
  
  in swishOpenFile fnam >>= maybe (return errVal) reader

-- | Open and read file, returning its handle and content, or Nothing
-- WARNING:  the handle must not be closed until input is fully evaluated
--
swishOpenFile :: Maybe String -> SwishStateIO (Maybe (Handle, Bool, String))
swishOpenFile Nothing     = readFromHandle stdin Nothing
swishOpenFile (Just fnam) = do
  o <- lift $ try $ openFile fnam ReadMode
  case o of
    Left  _ -> do
      swishError ("Cannot open file: "++fnam) SwishDataAccessError
      return Nothing
      
    Right hnd -> readFromHandle hnd $ Just ("file: " ++ fnam)

readFromHandle :: Handle -> Maybe String -> SwishStateIO (Maybe (Handle, Bool, String))
readFromHandle hdl mlbl = do
  hrd <- lift $ hIsReadable hdl
  if hrd
    then do
      fc <- lift $ hGetContents hdl
      return $ Just (hdl, isJust mlbl, fc)
  
    else do
      lbl <- case mlbl of
        Just l  -> lift (hClose hdl) >> return l
        Nothing -> return "standard input"
      swishError ("Cannot read from " ++ lbl) SwishDataAccessError
      return Nothing

swishParse :: 
  Maybe String -- ^ filename (if not stdin)
  -> String  -- ^ contents of file
  -> SwishStateIO (Maybe RDFGraph)
swishParse mfpath inp = do
  fmt <- gets format
  buri <- calculateBaseURI mfpath
  
  let toError eMsg =
        swishError (show fmt ++ " syntax error in " ++ inName ++ ": " ++ eMsg) SwishDataInputError 
        >> return Nothing
        
      inName = maybe "standard input" ("file " ++) mfpath
  
      readIn reader = case reader inp of
        Left eMsg -> toError eMsg
        Right res -> return $ Just res
             
  case fmt of
    N3 -> readIn (`parseN3` Just buri)
    NT -> readIn parseNT
    {-
    _  -> swishError ("Unsupported file format: "++show fmt) SwishArgumentError >>
          return Nothing
    -}
    
swishWriteFile :: 
  (Maybe String -> Handle -> SwishStateIO ()) -- ^ given a file name and a handle, write to it
  -> Maybe String
  -> SwishStateIO ()
swishWriteFile conv fnam =  
  let hdlr (h, c) = conv fnam h >> when c (lift $ hClose h)
  in swishCreateWriteableFile fnam >>= maybe (return ()) hdlr
   
-- | Open file for writing, returning its handle, or Nothing
--  Also returned is a flag indicating whether or not the
--  handled should be closed when writing is done (if writing
--  to standard output, the handle should not be closed as the
--  run-time system should deal with that).
swishCreateWriteableFile :: Maybe String -> SwishStateIO (Maybe (Handle,Bool))
swishCreateWriteableFile Nothing = do
  hwt <- lift $ hIsWritable stdout
  if hwt
    then return $ Just (stdout, False)
    else do
      swishError "Cannot write to standard output" SwishDataAccessError
      return Nothing
  
swishCreateWriteableFile (Just fnam) = do
  o <- lift $ try $ openFile fnam WriteMode
  case o of
    Left _ -> do
      swishError ("Cannot open file for writing: " ++ fnam) SwishDataAccessError
      return Nothing
      
    Right hnd -> do
      hwt <- lift $ hIsWritable hnd
      if hwt
        then return $ Just (hnd, True)
        else do
          lift $ hClose hnd
          swishError ("Cannot write to file: "++fnam) SwishDataAccessError
          return Nothing
  
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
