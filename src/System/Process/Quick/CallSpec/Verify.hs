module System.Process.Quick.CallSpec.Verify where

import Control.Monad.Writer.Strict hiding (lift)
import Data.Map qualified as M
import Data.Multimap.Table (row, rowKeys, rowKeysSet)
import Data.Set (findMin)
import Data.Text.Lazy qualified as LT
import System.Process.Quick.CallSpec.Verify.ImportOverlook
import Language.Haskell.TH.Syntax
import System.Process.Quick.CallSpec
import System.Process.Quick.CallSpec.Verify.Sandbox
import System.Process.Quick.CallSpec.Verify.TrailingHelp ( verifyTrailingHelp )
import System.Process.Quick.CallSpec.Verify.Type
import System.Process.Quick.Predicate (ArgCollector, RefinedInArgLocator(..), RefinedOutArgLocator(..))
import System.Process.Quick.Predicate.InDir ()
import System.Process.Quick.Predicate.InFile ()
import System.Process.Quick.Prelude hiding (Type, lift)
import System.Process.Quick.Prelude qualified as P
import System.Process.Quick.Util ( M )
import Text.Pretty.Simple


verifyWithActiveMethods ::
  forall w m cs. (M m, CallSpec cs, WriterT [FilePath] (CsPerfT m) ~ w) =>
  ArgCollector w ->
  ArgCollector w ->
  Set VerificationMethod ->
  Proxy cs ->
  Int ->
  CsPerfT m [CsViolationWithCtx]
verifyWithActiveMethods inArgLocators outArgLocators activeVerMethods pcs iterations =
  catMaybes <$> mapM go  (filter (`member` activeVerMethods) (verificationMethods pcs))
  where
    go :: VerificationMethod -> CsPerfT m (Maybe CsViolationWithCtx)
    go = \case
      TrailingHelpValidate ->
        measureX pcs TrailingHelpValidate #csTotalTime $
          P.lift (verifyTrailingHelp pcs iterations)
      SandboxValidate ->
        measureX pcs SandboxValidate #csTotalTime $
          validateInSandbox inArgLocators outArgLocators (generate (arbitrary @cs)) iterations

-- |Compose a list of monadic actions into one action.  Composes using
-- ('>=>') - that is, the output of each action is fed to the input of
-- the one after it in the list.
concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs = foldr (>=>) return fs

formatPerfReportLine :: (TypeRep, CsPerf) -> Doc
formatPerfReportLine (typR, csp) =
  hsep [ fill 29 $ pretty typR
       , fill 15 . pretty . getSum $ csTotalTime csp
       -- , pretty $ (getSum (csExeTime csp <> csGenerationTime csp) `div` iterations
       , fill 15 . pretty . getSum $ csGenerationTime csp
       , fill 15 . pretty . getSum $ csExeTime csp
       ]

reportFor :: MonadIO m => VerificationMethod -> CsPerfT m Doc
reportFor vm = do
  perfStats <- get
  pure $ tab (linebreak <+> "*** Method: " <+> pretty vm <> linebreak <>
       hsep [ fill 29 "Call Spec"
            , fill 15 "Total"
            , fill 15 "Generation"
            , fill 15 "Execution"
            ] $$
        "=======================================================================" $$
        (vcat . fmap formatPerfReportLine . reverse . sortWith (^. _2) . M.toList $ row vm perfStats)
      ) <> linebreak


consumeViolations :: MonadIO m => Int -> [CsViolationWithCtx] -> CsPerfT m ()
consumeViolations iterations = \case
  [] -> do
    perfStats <- get
    reports <- mapM reportFor $ rowKeys perfStats
    printDoc $ "All of" <+> pretty (M.size $ row (findMin $ rowKeysSet perfStats) perfStats)
      <+> "CallSpecs are valid due "
      <+> pretty iterations <+> "tests executed for each" <> fold reports

  vis -> do
    let dashes = "-------------------------------------------------------------"
    -- good case for hetftio ??
    printDoc $ "Error: quick-process found" <+> doc (length vis) <+> "failed call specs:"
      $$ (vcat $ zipWith (\i v -> tab ("-- [" <> doc i <> "]" <+> dashes $$ printViolation v))
                 [1::Int ..] (sortByProgamName vis))
      $$ "-------" <> dashes $$ "End of quick-process violation report" <> linebreak
    exitFailure
  where
    sortByProgamName = sortWith (\(CsViolationWithCtx x _) -> programName $ pure x)
    printViolation (CsViolationWithCtx cs v) =
      let pn = (text . toLazy . toText . programName $ pure cs) in
        case v of
          HelpKeyIgnored -> pn <> ": help key ignored"
          ProgramNotFound report' pathCopy ->
            "[" <> pn <> "] is not found on PATH:" $$ tab (vsep pathCopy)
             $$ "Report:" $$ tab report'
          HelpKeyNotSupported report' ->
            "--help key is not supported by [" <> pn <> "]"
            $$ "Report:" $$ tab report'
          HelpKeyExitNonZero rep ->
            pn <> " - non zero exit code:" $$ tab rep
          ExceptionThrown e ->
            "Launch of " <> pn <> " - thrown exception:" $$ tab (text $ show e)
            $$ "With arguments: " <> tab (programArgs cs)
          UnexpectedCallEffect uce -> do
            pn <> ": has unsafisfied effects:" $$ (vsep . fmap text . LT.lines $ pShow uce)
            $$ "With arguments: " <> tab (programArgs cs)

discoverAndVerifyCallSpecs :: Set VerificationMethod -> Int -> Q Exp
discoverAndVerifyCallSpecs activeVerMethods iterations = do
  startedAt <- runIO currentTime
  inArgLocators <- extractInstanceType <$> reifyInstances ''RefinedInArgLocator [VarT (mkName "b")]
  when (inArgLocators == []) $ putStrLn "Discovered 0 InArg locators!!!"
  outArgLocators <- extractInstanceType <$> reifyInstances ''RefinedOutArgLocator [VarT (mkName "c")]
  when (outArgLocators == []) $ putStrLn "Discovered 0 OutArg locators!!!"
  ts <- extractInstanceType <$> reifyInstances ''CallSpec [VarT (mkName "a")]
  when (ts == []) $ fail "Discovered 0 types with CallSpec instance!!!"
  overlookedCss <- verifyFoundCsCoverCompiledOnes ts
  when (overlookedCss /= mempty) . fail . toString . displayT . renderOneLine $
    "Overlooked CallSpecs: " <> pretty overlookedCss
  !r <- [| void $ runStateT (
             fmap concat
               (sequence $(ListE <$> (mapM (genCsVerification
                                            inArgLocators outArgLocators) ts))) >>=
             consumeViolations $(lift iterations))
             mempty
        |]
  endedAt <- runIO currentTime
  putStrLn $ "discoverAndVerifyCallSpecs generation took " <> show (endedAt `diffUTCTime` startedAt)
  pure r
  where
    getLocator n t = AppE (VarE n) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) t))

    pipeLocators :: Name -> [Type] -> Q Exp
    pipeLocators locName ts =
      [| concatM $(pure . ListE $ getLocator locName <$> ts) |]

    genCsVerification :: [Type] -> [Type] -> Type -> Q Exp
    genCsVerification inArL outArL t =
      [| verifyWithActiveMethods
           $(pipeLocators 'locateRefinedInArg inArL)
           $(pipeLocators 'locateRefinedOutArg outArL)
           $(lift activeVerMethods)
           $(pure $ SigE (ConE 'Proxy) (AppT (ConT ''Proxy) t))
           $(lift iterations)
       |]
    extractInstanceType :: [Dec] -> [Type]
    extractInstanceType = mapMaybe $ \case
      InstanceD _ _ (AppT _ t) _ ->
        Just t
      _ -> Nothing
