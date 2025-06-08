module System.Process.Quick.CallSpec.Verify.Sandbox where

import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Find as F
import Data.Conduit.List qualified as DCL
import Data.Multimap.Table qualified as T
import System.Directory
import System.FilePath (takeDirectory, takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Process.Quick.CallEffect
import System.Process.Quick.CallSpec
import System.Process.Quick.CallSpec.Verify.Type
import System.Process.Quick.Predicate
import System.Process.Quick.Predicate.InDir ()
import System.Process.Quick.Predicate.InFile ()
import System.Process.Quick.Prelude hiding (Type, lift)
import System.Process.Quick.Util


callProcessAndReport :: (CallSpec cs, M m) => cs -> m CsExecReport
callProcessAndReport cs = do
  proLaunchDir <- liftIO getCurrentDirectory
  startedAt <- currentTime
  liftIO (readProcessWithExitCode (programName (pure cs)) (programArgs cs) "") >>= \(ec, out, err) -> do
    endedAt <- currentTime
    pure CsExecReport
      { exitCode = ec
      , stdErr = err
      , stdOut = out
      , execTime = endedAt `diffUTCTime` startedAt
      , processDir = proLaunchDir
      }

normalizeOutcomeChecks :: CallSpec cs => cs -> [OutcomeChecker]
normalizeOutcomeChecks cs =
  case filter ecP origOutcomeChecks of
    [] -> ExitCodeEqualTo ExitSuccess : origOutcomeChecks
    [ExitCodeEqualTo ExitSuccess] ->
      $(tr "!ExitCodeEqualTo ExitSuccess check is redundant/cs")
        origOutcomeChecks
    [ExitCodeEqualTo _] -> origOutcomeChecks
    _ ->
      $(tr "!Multiple ExitCodeEqualTo checks/cs")
        origOutcomeChecks
  where
    origOutcomeChecks = outcomeCheckers cs
    ecP = \case ExitCodeEqualTo _ -> True ; _ -> False


measureX :: forall m cs a. (Typeable cs, M m) =>
  Proxy cs -> VerificationMethod -> Lens' CsPerf (Sum NominalDiffTime) -> CsPerfT m a -> CsPerfT m a
measureX pcs vm l a = do
  started <- currentTime
  !r <- a
  ended <- currentTime
  modify' $ T.alter (merge ended started) vm (typeRep pcs)
  pure r
  where
    merge e s = pure . (l .~ (Sum $ e `diffUTCTime` s)) . fromMaybe mempty

validateInSandbox ::
  forall w m cs. (M m, CallSpec cs, WriterT [FilePath] (CsPerfT m) ~ w) =>
  ArgCollector w ->
  ArgCollector w ->
  Proxy cs ->
  Int ->
  CsPerfT m (Maybe CsViolationWithCtx)
validateInSandbox inArgLocators outArgLocators pcs !iterations
  | iterations <= 0 = pure Nothing
  | otherwise =
    withSystemTempDirectory "quick-process" go >>= \case
      Nothing -> validateInSandbox inArgLocators outArgLocators pcs $ iterations - 1
      Just e -> pure $ Just e
  where
    checkFilesExist cs outFiles = do
      filterM (pure . not <=< doesFileExist) outFiles >>= \case
        [] -> pure Nothing
        ne -> pure . Just . CsViolationWithCtx cs $
          UnexpectedCallEffect
          [ FsEffect . FsAnd $ fmap (FsNot . flip FsPathPredicate [FsExists]) ne
          ]

    findOriginFor projectDir inFile = do
      xs :: [FilePath] <- runConduitRes $
        F.find projectDir (do ignoreVcs
                              glob $ "*" <> takeExtension inFile
                              regular
                              not_ F.executable) .| DCL.consume
      case xs of
        [] -> pure Nothing
        neXs -> Just <$> generate (elements neXs)

    genInputFile projectDir inFile = (fromMaybe "/etc/hosts" <$> findOriginFor projectDir inFile) >>=
      \origin -> createDirectoryIfMissing True (takeDirectory inFile) >>
                 copyFile origin inFile
                 -- putStrLn ("File "  <> show origin <> " => " <> show inFile)

    doIn projectDir () = do
      cs <- measureX pcs SandboxValidate #csGenerationTime (liftIO (generate (arbitrary @cs)))
      inFiles <- execWriterT (gmapM inArgLocators cs)
      -- absolute path is an issue for generator
      -- though process in docker is run under root - high chance to pass ;)
      -- quick hack is to use  odd size in Gen to avoid absolute path it Sandbox mode
      mapM_ (liftIO1 (genInputFile projectDir)) inFiles
      let nocs = normalizeOutcomeChecks cs
      tryIO (measureX pcs SandboxValidate #csExeTime $ callProcessAndReport cs) >>= \case
        Left e -> throw . CsViolationWithCtx cs . ExceptionThrown $ SomeException e
        Right csr -> mapM (check csr) nocs >>= pure . concat >>=
          \case
            [] -> do
              outFiles <- execWriterT (gmapM outArgLocators cs)
              liftIO (checkFilesExist cs outFiles)
            cfs -> pure . Just . CsViolationWithCtx cs $ UnexpectedCallEffect cfs
    go tdp = do
      projectDir <- liftIO getCurrentDirectory
      bracket
        (liftIO $ setCurrentDirectory tdp)
        (\() -> liftIO $ setCurrentDirectory projectDir)
        (doIn projectDir)
