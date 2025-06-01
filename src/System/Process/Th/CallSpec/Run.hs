module System.Process.Th.CallSpec.Run where

import System.Process.Th.Prelude
import System.Process.Th.CallSpec.Type
import System.Process qualified as SP

-- | See 'SP.callProcess'
callProcess :: (MonadIO m, CallSpec cs) => cs -> m ()
callProcess cs = liftIO $ SP.callProcess (programName $ pure cs) (programArgs cs)

-- | See 'SP.spawnProcess'
spawnProcess :: (MonadIO m, CallSpec cs) => cs -> m SP.ProcessHandle
spawnProcess cs = liftIO $ SP.spawnProcess (programName $ pure cs) (programArgs cs)

-- | See 'SP.readProcess'
readProcess :: (MonadIO m, CallSpec cs) => cs -> String -> m String
readProcess cs input = liftIO $ SP.readProcess (programName $ pure cs) (programArgs cs) input

-- | See 'SP.readProcessWithExitCode'
readProcessWithExitCode :: (MonadIO m, CallSpec cs) => cs -> String -> m (ExitCode, String, String)
readProcessWithExitCode cs input = liftIO $ SP.readProcessWithExitCode (programName $ pure cs) (programArgs cs) input

-- | See 'SP.proc'
proc :: CallSpec cs => cs -> CreateProcess
proc cs = SP.proc (programName $ pure cs) (programArgs cs)
