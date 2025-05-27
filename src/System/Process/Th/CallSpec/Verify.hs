{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MonoLocalBinds #-}
module System.Process.Th.CallSpec.Verify where


import Control.Monad.Writer.Strict hiding (lift)
import Data.Conduit ( runConduitRes, (.|) )
import Data.Conduit.Find as F
import Data.Conduit.List qualified as DCL
import Debug.TraceEmbrace
import Language.Haskell.TH.Syntax
import System.Directory
import System.Exit hiding (exitFailure)
import System.FilePath (getSearchPath, takeDirectory, takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Process.Th.CallEffect
import System.Process.Th.CallSpec
import System.Process.Th.Predicate.InputFile
import System.Process.Th.Prelude hiding (Type, lift)


type FailureReport = Doc

data CallSpecViolation
  = HelpKeyIgnored
  | HelpKeyNotSupported FailureReport
  | ProgramNotFound FailureReport [FilePath]
  | HelpKeyExitNonZero FailureReport
  | SandboxLaunchFailed FailureReport
  | UnexpectedCallEffect [CallEffect]
  deriving (Show, Eq)

data CsViolationWithCtx
  = forall cs. CallSpec cs
  => CsViolationWithCtx
     { csContext :: cs
     , csViolation :: CallSpecViolation
     }

type M m = (MonadMask m, MonadCatch m, MonadIO m)


callProcessSilently :: M m => FilePath -> [String] -> m (Maybe Doc)
callProcessSilently p args =
  tryIO (liftIO (readProcessWithExitCode p args "")) >>= \case
    Left e ->
      pure . Just $ "Command: [" <> doc p <> " " <> hsep (escArg <$> args) <> "]" $$
      "Failed due:" $$ tab e

    Right (ExitSuccess, _, _) -> pure Nothing
    Right (ExitFailure ec, out, err) ->
      pure . Just $ "Command: [" <> doc p <> " " <> hsep (escArg <$> args) <> "]" $$
      (if ec > 1 then "Exited with: " <> show ec $$ "" else "")
      <> out &! (("Output: " <+>) . tab) <> err &! (("StdErr: " <+>) . tab)

verifyWithActiveMethods ::
  forall m cs. (M m, CallSpec cs) =>
  Set VerificationMethod ->
  Proxy cs ->
  Int ->
  m [CsViolationWithCtx]
verifyWithActiveMethods activeVerMethods pcs iterations =
  catMaybes <$> mapM go  (filter (`member` activeVerMethods) (verificationMethods pcs))
  where
    go = \case
      TrailingHelpValidate -> verifyTrailingHelp pcs iterations
      SandboxValidate -> validateInSandbox pcs iterations

validateInSandbox ::
  forall m cs. (M m, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
validateInSandbox pcs !iterations
  | iterations <= 0 = pure Nothing
  | otherwise =
    withSystemTempDirectory "th-process" go >>= \case
      Nothing -> validateInSandbox pcs $ iterations - 1
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
                 copyFile origin inFile >>
                 putStrLn ("File "  <> show origin <> " => " <> show inFile)

    doIn projectDir () = do
      cs <- liftIO (generate (arbitrary @cs))
      inFiles <- execWriterT (gmapM findInFile cs)
      -- absolute path is an issue for generator
      -- though process in docker is run under root - high chance to pass ;)
      -- quick hack is to use  odd size in Gen to avoid absolute path it Sandbox mode
      mapM_ (liftIO1 (genInputFile projectDir)) inFiles
      callProcessSilently (programName (pure cs)) (programArgs cs) >>= \case
        Nothing -> do
          outFiles <- execWriterT (gmapM findOutFile cs)
          liftIO (checkFilesExist cs outFiles)
        Just e -> pure . Just . CsViolationWithCtx cs $ SandboxLaunchFailed e
    go tdp = do
      projectDir <- liftIO getCurrentDirectory
      bracket
        (liftIO $ setCurrentDirectory tdp)
        (\() -> liftIO $ setCurrentDirectory projectDir)
        (doIn projectDir)

verifyTrailingHelp ::
  forall m cs. (M m, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
verifyTrailingHelp pcs iterations =
  callProcessSilently "which"  [programName pcs] >>= \case
    Just rep -> do
      cs <- genCs
      Just . CsViolationWithCtx cs . ProgramNotFound rep <$> liftIO getSearchPath
    Nothing ->
      spCmd (programName pcs) helpKey
        (spCmd (programName pcs) ("--hheellppaoesnthqkxsth" : helpKey)
           (do cs <- genCs
               pure . Just $ CsViolationWithCtx cs HelpKeyIgnored)
           (\_ -> go iterations))
        (\rep -> do
            cs <- genCs
            pure . Just . CsViolationWithCtx cs $ HelpKeyNotSupported rep)
  where
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


consumeViolations :: MonadIO m => [CsViolationWithCtx] -> m ()
consumeViolations = \case
  [] ->
    putStrLn "CallSpecs are valid"
  vis -> do
    let dashes = "-------------------------------------------------------------"
    -- good case for hetftio ??
    printDoc $ "Error: quick-process found " <> doc (length vis) <> " failed call specs:"
      $$ (vcat $ zipWith (\i v -> tab ("-- [" <> doc i <> "] " <> dashes $$ printViolation v))
                 [1::Int ..] (sortByProgamName vis))
      <> "---------" <> dashes $$ "End of quick-process violation report"
    exitFailure
  where
    sortByProgamName = sortWith (\(CsViolationWithCtx x _) -> programName $ pure x)
    printViolation (CsViolationWithCtx cs v) =
      case v of
        HelpKeyIgnored -> (text . programName $ pure cs) <> ": help key ignored"
        ProgramNotFound report' pathCopy ->
          (text . programName $ pure cs) <> " is not found on PATH:" $$ vsep pathCopy
           $$ "Report:" $$ tab report'
        HelpKeyNotSupported report' ->
          "--help key is not supported by [" <> (text . programName $ pure cs) <> "]"
          $$ "Report:" $$ tab report'
        HelpKeyExitNonZero rep ->
          (text . programName $ pure cs) <> " - non zero exit code:" $$ tab rep
        SandboxLaunchFailed rep ->
          (text . programName $ pure cs) <> " - non zero exit code:" $$ tab rep
        UnexpectedCallEffect uce -> do
          (text . programName $ pure cs) <> ": has unsafisfied effects:" $$ (text $ show uce)
           $$ "With arguments: " <> tab (programArgs cs)

discoverAndVerifyCallSpecs :: Set VerificationMethod -> Int -> Q Exp
discoverAndVerifyCallSpecs activeVerMethods iterations = do
  ts <- extractInstanceType <$> reifyInstances ''CallSpec [VarT (mkName "a")]
  when (ts == []) $ putStrLn "Discovered 0 types with CallSpec instance!!!"
  [| fmap concat (sequence $(ListE <$> (mapM genCsVerification ts))) >>= consumeViolations |]
  where
    genCsVerification :: Type -> Q Exp
    genCsVerification t =
      [| verifyWithActiveMethods
           $(lift activeVerMethods)
           $(pure $ SigE (ConE 'Proxy) (AppT (ConT ''Proxy) t))
           $(lift iterations)
       |]
    extractInstanceType :: [Dec] -> [Type]
    extractInstanceType = mapMaybe $ \case
      InstanceD _ _ (AppT _ t) _ ->
        Just t
      _ -> Nothing
