{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Process.Th.CallSpec.Verify where

import Language.Haskell.TH.Syntax
import System.Exit hiding (exitFailure)
import System.FilePath (getSearchPath)
import System.Process (spawnProcess, system, waitForProcess)
import System.Process.Th.CallEffect
import System.Process.Th.CallSpec
import System.Process.Th.Prelude hiding (Type, lift)
import System.Random
import Test.QuickCheck


data Verification
  = TrailingHelpValidate
  | PureCall
  | SandboxSafe
  deriving (Show, Eq)

data CallSpecViolation
  = HelpKeyIgnored
  | ProgramNotFound [FilePath]
  | HelpKeyExitNonZero Int
  | UnexpectedCallEffect [CallEffect]
  deriving (Show, Eq)

data CsViolationWithCtx
  = forall cs. CallSpec cs
  => CsViolationWithCtx
     { csContext :: cs
     , csViolation :: CallSpecViolation
     }

verifyTrailingHelp ::
  forall m cs. (MonadIO m, Random cs, CallSpec cs) =>
  Proxy cs ->
  Int ->
  m (Maybe CsViolationWithCtx)
verifyTrailingHelp _ = go
  where
    helpKey = ["--help"]
    spCmd pn args onSuccess onFailure =
      (liftIO $ spawnProcess pn args) >>= liftIO1 waitForProcess >>= \case
        ExitSuccess -> onSuccess
        ExitFailure ec -> onFailure ec
    go n
      | n <= 0 = pure Nothing
      | otherwise = do
          cs <- liftIO (generate (chooseAny @cs))
          liftIO (system ("which " <> (programName cs))) >>= \case
            ExitFailure _ ->
              Just . CsViolationWithCtx cs . ProgramNotFound <$> liftIO getSearchPath
            ExitSuccess ->
              spCmd (programName cs) ("--hheellppaoesnthqkxsth" : helpKey)
                (pure . Just $ CsViolationWithCtx cs HelpKeyIgnored)
                (\_ ->
                   spCmd (programName cs) (programArgs cs <> helpKey)
                     (go $ n - 1)
                     (\ec -> pure . Just . CsViolationWithCtx cs $ HelpKeyExitNonZero ec))

consumeViolations :: MonadIO m => [CsViolationWithCtx] -> m ()
consumeViolations = \case
  [] ->
    pure ()
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
        ProgramNotFound pathCopy ->
          putStrLn $ (programName cs) <> " is not found on PATH " <> show pathCopy
        HelpKeyExitNonZero ec -> do
          putStrLn $ (programName cs) <> ": non zero exit code (" <> show ec <> ")"
          putStrLn $ "    with arguments: " <> show (programArgs cs)
        UnexpectedCallEffect uce -> do
          putStrLn $ (programName cs) <> ": has unsafisfied effects: " <> show uce
          putStrLn $ "    with arguments: " <> show (programArgs cs)

discoverAndVerifyCallSpecs :: Int -> Q Exp
discoverAndVerifyCallSpecs iterations = do
  ts <- extractInstanceType <$> reifyInstances ''CallSpec [VarT (mkName "a")]
  when (ts == []) $ putStrLn "Discovered 0 types with CallSpec instance!!!"
  [| fmap catMaybes (sequence $(ListE <$> (mapM genCsVerification ts)))  >>= consumeViolations |]
  where
    genCsVerification :: Type -> Q Exp
    genCsVerification t =
      [| verifyTrailingHelp
           $(pure $ SigE (ConE ''Proxy) (AppT (ConT ''Proxy) t))
           $(lift iterations)
       |]
    extractInstanceType :: [Dec] -> [Type]
    extractInstanceType = mapMaybe $ \case
      InstanceD _ _ (AppT _ t) _ ->
        Just t
      _ -> Nothing
