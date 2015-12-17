{-# LANGUAGE OverloadedStrings #-}
module Process where
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V4 (nextRandom)
import System.Process (createProcess, shell, waitForProcess, terminateProcess, ProcessHandle)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Text.Lazy (Text, unpack, toStrict)
import qualified Data.Text as T (Text)
import Data.Maybe (fromMaybe)

-- | Structure that keeps truck of the current processes.
-- The first field is the count of updates to the data structure.
-- The second field is the map that holds process descriptors keyed
-- by UUIDs.
data Processes = Processes Int (Map.Map UUID ProcessHandle)

type AppState = TVar Processes

-- | Initialization of application state
startAppState :: IO AppState
startAppState = newTVarIO (Processes 1 Map.empty)

-- | Runs a shell command. Stores the value of the process handle
-- in a Map, keyed by a UUID it generates internally. Returns the
-- generated UUID.
runCommand :: AppState -> Text -> IO T.Text
runCommand psVar cmd = do (_, _, _, ph) <- createProcess (shell $ unpack cmd)
                          uuid <- nextRandom
                          atomically $ insertNewProcess uuid ph
                          forkIO (waitForProcess ph >> (atomically $ deleteProcess uuid))
                          return $ toText uuid
  where
    insertNewProcess :: UUID -> ProcessHandle -> STM ()
    insertNewProcess uuid ph = do
      (Processes crt ps) <- readTVar psVar
      writeTVar psVar (Processes (crt + 1) (Map.insert uuid ph ps))
    deleteProcess :: UUID -> STM ()
    deleteProcess uuid = do
      (Processes crt ps) <- readTVar psVar
      writeTVar psVar (Processes (crt + 1) (Map.delete uuid ps))

-- | Waits for the next update of the processes map and returns
-- the list of process UUIDs that are still running.
getNextUpdate :: AppState
              -> Int -- ^ The current state
              -> IO [T.Text]
getNextUpdate processesVar known = atomically (readTVar processesVar >>= getNextUpdate_)
  where
    getNextUpdate_ (Processes crt ps) = do
      if known >= crt
        then retry
        else return $ map toText $ Map.keys ps

-- | Terminates a process identified by a given UUID
terminateCommand :: AppState -> Text -> IO ()
terminateCommand psVar uuid = do
  (Processes _ ps) <- atomically (readTVar psVar)
  fromMaybe ((return ()) :: IO ()) $ do
    uuid' <- fromText $ toStrict uuid
    ph <- Map.lookup uuid' ps
    return $ terminateProcess ph
