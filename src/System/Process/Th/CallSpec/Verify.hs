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
import Test.QuickCheck


data Verification
  = TrailingHelpValidate
  | PureCall
  | SandboxSafe
  deriving (Show, Eq)

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

callProcessSilently :: MonadIO m => FilePath -> [String] -> m (Maybe String)
callProcessSilently p args =
  liftIO (readProcessWithExitCode p args "") >>= \case
    (ExitSuccess, _, _) -> pure Nothing
    (ExitFailure ec, out, err) ->
      pure . Just $ "Command: " <> p <> " " <> intercalate " " args <>
      "\nExited with: " <> show ec <> "\nOutput:\n" <> out <> "\nStdErr:\n" <> err

verifyTrailingHelp ::
  forall m cs. (MonadIO m, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
verifyTrailingHelp _ = go
  where
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
          callProcessSilently "which"  [programName cs] >>= \case
            Just rep ->
              Just . CsViolationWithCtx cs . ProgramNotFound rep <$> liftIO getSearchPath
            Nothing ->
              spCmd (programName cs) helpKey
                (spCmd (programName cs) ("--hheellppaoesnthqkxsth" : helpKey)
                   (pure . Just $ CsViolationWithCtx cs HelpKeyIgnored)
                   (\_ ->
                      spCmd (programName cs) (programArgs cs <> helpKey)
                        (go $ n - 1)
                        (\rep -> pure . Just . CsViolationWithCtx cs $ HelpKeyExitNonZero rep)))
                (\rep -> pure . Just . CsViolationWithCtx cs $ HelpKeyNotSupported rep)

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
    sortByProgamName = sortWith (\(CsViolationWithCtx x _) -> programName x)
    printViolation (CsViolationWithCtx cs v) =
      case v of
        HelpKeyIgnored ->
          putStrLn $ (programName cs) <> ": help key ignored"
        ProgramNotFound report' pathCopy ->
          putStrLn $ (programName cs) <> " is not found on PATH " <> show pathCopy <> "\nReport:\n" <> report'
        HelpKeyNotSupported report' ->
          putStrLn $ "--help key is not supported by " <> programName cs <> "\nReport:\n" <> report'
        HelpKeyExitNonZero rep -> do
          putStrLn $ (programName cs) <> ": non zero exit code (" <> rep <> ")"
          putStrLn $ "    with arguments: " <> show (programArgs cs)
        UnexpectedCallEffect uce -> do
          putStrLn $ (programName cs) <> ": has unsafisfied effects: " <> show uce
          putStrLn $ "    with arguments: " <> show (programArgs cs)

discoverAndVerifyCallSpecs :: Int -> Q Exp
discoverAndVerifyCallSpecs iterations = do
  ts <- extractInstanceType <$> reifyInstances ''CallSpec [VarT (mkName "a")]
  when (ts == []) $ putStrLn "Discovered 0 types with CallSpec instance!!!"
  [| fmap catMaybes (sequence $(ListE <$> (mapM genCsVerification ts))) >>= consumeViolations |]
  where
    genCsVerification :: Type -> Q Exp
    genCsVerification t =
      [| verifyTrailingHelp
           $(pure $ SigE (ConE 'Proxy) (AppT (ConT ''Proxy) t))
           $(lift iterations)
       |]
    extractInstanceType :: [Dec] -> [Type]
    extractInstanceType = mapMaybe $ \case
      InstanceD _ _ (AppT _ t) _ ->
        Just t
      _ -> Nothing
