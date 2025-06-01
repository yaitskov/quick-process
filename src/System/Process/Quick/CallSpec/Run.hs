module System.Process.Quick.CallSpec.Run where

import System.Process.Quick.Prelude
import System.Process.Quick.CallSpec.Type
import System.Process qualified as SP

-- | See 'System.Process.callProcess'
callProcess :: (MonadIO m, CallSpec cs) => cs -> m ()
callProcess cs = liftIO $ SP.callProcess (programName $ pure cs) (programArgs cs)

-- | See 'System.Process.spawnProcess'
spawnProcess :: (MonadIO m, CallSpec cs) => cs -> m SP.ProcessHandle
spawnProcess cs = liftIO $ SP.spawnProcess (programName $ pure cs) (programArgs cs)

-- | See 'System.Process.readProcess'
readProcess :: (MonadIO m, CallSpec cs) => cs -> String -> m String
readProcess cs input = liftIO $ SP.readProcess (programName $ pure cs) (programArgs cs) input

-- | See 'System.Process.readProcessWithExitCode'
readProcessWithExitCode :: (MonadIO m, CallSpec cs) => cs -> String -> m (ExitCode, String, String)
readProcessWithExitCode cs input = liftIO $ SP.readProcessWithExitCode (programName $ pure cs) (programArgs cs) input

-- | See 'System.Process.proc'
proc :: CallSpec cs => cs -> CreateProcess
proc cs = SP.proc (programName $ pure cs) (programArgs cs)
