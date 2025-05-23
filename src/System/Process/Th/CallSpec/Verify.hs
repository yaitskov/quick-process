{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Process.Th.CallSpec.Verify where

import Debug.TraceEmbrace
import Language.Haskell.TH.Syntax
import System.Exit hiding (exitFailure)
import System.FilePath (getSearchPath)
import System.Process (readProcessWithExitCode)
import System.Process.Th.CallEffect
import System.Process.Th.CallSpec
import System.Process.Th.Prelude hiding (Type, lift)

type FailureReport = String

data CallSpecViolation
  = HelpKeyIgnored
  | HelpKeyNotSupported FailureReport
  | ProgramNotFound FailureReport [FilePath]
  | HelpKeyExitNonZero FailureReport
  | UnexpectedCallEffect [CallEffect]
  deriving (Show, Eq)

data CsViolationWithCtx
  = forall cs. CallSpec cs
  => CsViolationWithCtx
     { csContext :: cs
     , csViolation :: CallSpecViolation
     }

callProcessSilently :: (MonadCatch m, MonadIO m) => FilePath -> [String] -> m (Maybe String)
callProcessSilently p args =
  tryIO (liftIO (readProcessWithExitCode p args "")) >>= \case
    Left e ->
      pure . Just $ "Command: " <> p <> " " <> intercalate " " args <>
      "\nFailed due:\n" <> show e

    Right (ExitSuccess, _, _) -> pure Nothing
    Right (ExitFailure ec, out, err) ->
      pure . Just $ "Command: " <> p <> " " <> intercalate " " args <>
      "\nExited with: " <> show ec <> "\nOutput:\n" <> out <> "\nStdErr:\n" <> err

verifyWithActiveMethods ::
  forall m cs. (MonadCatch m, MonadIO m, CallSpec cs) =>
  Set VerificationMethod ->
  Proxy cs ->
  Int ->
  m [CsViolationWithCtx]
verifyWithActiveMethods activeVerMethods pcs iterations =
  catMaybes <$> mapM  go  (filter (`member` activeVerMethods) (verificationMethods pcs))
  where
    go = \case
      TrailingHelpValidate -> verifyTrailingHelp pcs iterations
      SandboxValidate -> validateInSandbox pcs iterations

validateInSandbox ::
  forall m cs. (MonadCatch m, MonadIO m, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
validateInSandbox _ _ = pure Nothing

verifyTrailingHelp ::
  forall m cs. (MonadCatch m, MonadIO m, CallSpec cs) =>
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
    -- good case for hetftio ??
    putStrLn $ "Error: quick-process found " <> show (length vis) <> " failed call specs:"
    forM_ (sortByProgamName vis) printViolation
    putStrLn "End of quick-process violation report"
    exitFailure
  where
    sortByProgamName = sortWith (\(CsViolationWithCtx x _) -> programName $ pure x)
    printViolation (CsViolationWithCtx cs v) =
      case v of
        HelpKeyIgnored ->
          putStrLn $ (programName $ pure cs) <> ": help key ignored"
        ProgramNotFound report' pathCopy ->
          putStrLn $ (programName $ pure cs) <> " is not found on PATH " <> show pathCopy <> "\nReport:\n" <> report'
        HelpKeyNotSupported report' ->
          putStrLn $ "--help key is not supported by " <> programName (pure cs) <> "\nReport:\n" <> report'
        HelpKeyExitNonZero rep -> do
          putStrLn $ (programName $ pure cs) <> ": non zero exit code (" <> rep <> ")"
          putStrLn $ "    with arguments: " <> show (programArgs cs)
        UnexpectedCallEffect uce -> do
          putStrLn $ (programName $ pure cs) <> ": has unsafisfied effects: " <> show uce
          putStrLn $ "    with arguments: " <> show (programArgs cs)

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
