--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  SwishCommands
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
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
    , stdin, stdout, stderr
    )

import Network.URI (URI, 
                    relativeTo,
                    parseURI, parseURIReference, uriToString)

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (modify, gets)
import Control.Monad (liftM, when)

import System.IO.Error

------------------------------------------------------------
--  Set file format to supplied value
------------------------------------------------------------

swishFormat :: SwishFormat -> SwishStateIO ()
swishFormat = modify . setFormat

------------------------------------------------------------
--  Set base URI to supplied value
------------------------------------------------------------

swishBase :: Maybe QName -> SwishStateIO ()
swishBase = modify . setBase

{-

------------------------------------------------------------
--  Display the banner or not
------------------------------------------------------------

swishVerbose :: Bool -> SwishStateIO ()
swishVerbose = modify . setVerbose
-}

------------------------------------------------------------
--  Read graph from named file
------------------------------------------------------------

swishInput :: String -> SwishStateIO ()
swishInput fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . setGraph)
  
------------------------------------------------------------
--  Merge graph from named file
------------------------------------------------------------

swishMerge :: String -> SwishStateIO ()
swishMerge fnam =
  swishReadGraph fnam >>= maybe (return ()) (modify . mergeGraph)
    
mergeGraph :: RDFGraph -> SwishState -> SwishState
mergeGraph gr state = state { graph = newgr }
    where
        newgr = merge gr (graph state)

------------------------------------------------------------
--  Compare graph from named file
------------------------------------------------------------

swishCompare :: String -> SwishStateIO ()
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

swishGraphDiff :: String -> SwishStateIO ()
swishGraphDiff fnam =
  swishReadGraph fnam >>= maybe (return ()) diffGraph

diffGraph :: RDFGraph -> SwishStateIO ()
diffGraph gr = do
  oldGr <- gets graph
  let p1 = partitionGraph (getArcs oldGr)
      p2 = partitionGraph (getArcs gr)
      diffs = comparePartitions p1 p2
  maybehandleclose <- swishWriteFile "" -- null filename -> stdout
  case maybehandleclose of
    Just (h,c) -> do
      swishOutputDiffs "" h diffs
      when c (lift $ hClose h)
   
    _  -> return ()
  
swishOutputDiffs :: (Label lb) =>
    String -> Handle
    -> [(Maybe (GraphPartition lb),Maybe (GraphPartition lb))]
    -> SwishStateIO ()
swishOutputDiffs fnam hnd diffs = do
  lift $ hPutStrLn hnd ("Graph differences: "++show (length diffs))
  mapM_ (swishOutputDiff fnam hnd) (zip [1..] diffs)

swishOutputDiff :: (Label lb) =>
    String -> Handle
    -> (Int,(Maybe (GraphPartition lb),Maybe (GraphPartition lb)))
    -> SwishStateIO ()
swishOutputDiff fnam hnd (diffnum,(part1,part2)) = do
  lift $ hPutStrLn hnd ("---- Difference "++show diffnum++" ----")
  lift $ hPutStr hnd "Graph 1:"
  swishOutputPart fnam hnd part1
  lift $ hPutStr hnd "Graph 2:"
  swishOutputPart fnam hnd part2

swishOutputPart :: (Label lb) =>
    String -> Handle -> Maybe (GraphPartition lb) -> SwishStateIO ()
swishOutputPart _ hnd part = 
  let out = maybe "\n(No arcs)" (partitionShowP "\n") part
  in lift $ hPutStrLn hnd out

------------------------------------------------------------
--  Execute script from named file
------------------------------------------------------------

swishScript :: String -> SwishStateIO ()
swishScript fnam = swishReadScript fnam >>= mapM_ swishCheckResult

swishReadScript :: String -> SwishStateIO [SwishStateIO ()]
swishReadScript fnam =
  let hdlr (h,i,mfpath) = do
        res <- swishParseScript (mfpath,fnam) i
        lift $ hClose h
        return res
  
  in swishOpenFile fnam >>= maybe (return []) hdlr

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
  
calculateBaseURI Nothing = maybe defURI id `liftM` gets base
    
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
    (Maybe FilePath, String) -- file name as QName, file name (may be "")
    -> String  -- script contents
    -> SwishStateIO [SwishStateIO ()]
swishParseScript (mfpath,fnam) inp = do
  buri <- calculateBaseURI mfpath
  case parseScriptFromString (Just buri) inp of
    Left err -> do
      swishError ("Script syntax error in file "++fnam++": "++err) SwishDataInputError
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

swishOutput :: String -> SwishStateIO ()
swishOutput fnam = 
  let hdlr (h,c) = swishOutputGraph fnam h >> when c (lift $ hClose h)
  in swishWriteFile fnam >>= maybe (return ()) hdlr
     
swishOutputGraph :: String -> Handle -> SwishStateIO ()
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

swishReadGraph :: String -> SwishStateIO (Maybe RDFGraph)
swishReadGraph fnam =
  let reader (h,i,mfpath) = do
        res <- swishParse (mfpath,fnam) i
        lift $ hClose h
        return res
  
  in swishOpenFile fnam >>= maybe (return Nothing) reader

-- Open and read file, returning its handle and content, or Nothing
-- WARNING:  the handle must not be closed until input is fully evaluated
--
-- The file name is also returned (or None if stdin is being
-- used). The file name is not 'normalized' here (i.e. relative paths
-- are not expanded out here).
--
swishOpenFile :: String -> SwishStateIO (Maybe (Handle,String,Maybe FilePath))
swishOpenFile fnam = do
  (hnd,hop,qn) <- lift $ if null fnam
                      then return (stdin,True,Nothing)
                      else do
                        o <- try (openFile fnam ReadMode)
                        case o of
                          Left  _ -> return (stdin,False,Nothing)
                          Right h -> return (h,True,Just fnam)

  hrd <- lift $ hIsReadable hnd
  if hop && hrd
    then do
      fc <- lift $ hGetContents hnd
      return $ Just (hnd,fc,qn)
  
    else do
      lift $ hClose hnd
      swishError ("Cannot read file: "++fnam) SwishDataAccessError
      return Nothing


swishParse :: 
  (Maybe FilePath, String) -- ^ base of file, file name (may be "")
  -> String  -- ^ contents of file
  -> SwishStateIO (Maybe RDFGraph)
swishParse (mfpath,fnam) inp = do
  fmt <- gets format
  buri <- calculateBaseURI mfpath
  
  let toError eMsg =
        swishError (show fmt ++ " syntax error in file " ++ fnam ++ ": " ++ eMsg) SwishDataInputError 
        >> return Nothing
        
      readIn reader = case reader inp of
        Left eMsg -> toError eMsg
        Right res -> return $ Just res
             
  case fmt of
    N3 -> readIn (flip parseN3 (Just buri))
    NT -> readIn parseNT
    {-
    _  -> swishError ("Unsupported file format: "++show fmt) SwishArgumentError >>
          return Nothing
    -}
    
--  Open file for writing, returning its handle, or Nothing
--  Also returned is a flag indicating whether or not the
--  handled should be closed when writing is done (if writing
--  to standard output, the handle should not be closed as the
--  run-time system should deal with that).
swishWriteFile :: String -> SwishStateIO (Maybe (Handle,Bool))
swishWriteFile fnam = do
  (hnd,hop,cls) <-
    lift $ if null fnam 
           then return (stdout,True,False)
           else do
             o <- try (openFile fnam WriteMode)
             case o of
               Left  _ -> return (stderr,False,False)
               Right h -> return (h,True,True)
            
  hwt <- lift $ hIsWritable hnd
  if hop && hwt
    then return $ Just (hnd,cls)
    else do
      when cls (lift $ hClose hnd)
      swishError ("Cannot write file: "++fnam) SwishDataAccessError
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
