module System.Process.Quick.CallSpec.Verify.TrailingHelp where


import Debug.TraceEmbrace ( trIo )
import System.FilePath ( getSearchPath )
import System.Directory ( findExecutable )
import System.Process.Quick.CallSpec
    ( CallSpec(programArgs, programName) )
import System.Process.Quick.CallSpec.Verify.Type
    ( CsViolationWithCtx(CsViolationWithCtx),
      CallSpecViolation(HelpKeyNotSupported, HelpKeyExitNonZero,
                        ProgramNotFound, HelpKeyIgnored) )
import System.Process.Quick.Predicate.InDir ()
import System.Process.Quick.Predicate.InFile ()
import System.Process.Quick.Prelude hiding (Type, lift)
import System.Process.Quick.Util ( callProcessSilently, M )

verifyTrailingHelp ::
  forall m cs. (M m, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
verifyTrailingHelp pcs iterations =
  liftIO (findExecutable progName) >>= \case
    Nothing -> do
      cs <- genCs
      Just . CsViolationWithCtx cs . ProgramNotFound (text . toLazy $ toText progName) <$> liftIO getSearchPath
    Just _ -> do
      spCmd progName helpKey
        (spCmd progName ("--hheellppaoesnthqkxsth" : helpKey)
           (do cs <- genCs
               pure . Just $ CsViolationWithCtx cs HelpKeyIgnored)
           (\_ -> go iterations))
        (\rep -> do
            cs <- genCs
            pure . Just . CsViolationWithCtx cs $ HelpKeyNotSupported rep)
  where
    progName = programName pcs
    genCs = liftIO (generate (arbitrary @cs))
    helpKey = ["--help"]
    spCmd pn args onSuccess onFailure = do
      liftIO $(trIo "spawn process/pn args")
      callProcessSilently pn args >>= \case
        Nothing -> onSuccess
        Just rep -> onFailure rep
    go n
      | n <= 0 = pure Nothing
      | otherwise = do
          cs <- liftIO (generate (arbitrary @cs))
          spCmd (programName pcs) (programArgs cs <> helpKey)
            (go $ n - 1)
            (\rep -> pure . Just . CsViolationWithCtx cs $ HelpKeyExitNonZero rep)
