{-# OPTIONS_GHC -Wno-orphans #-}

module System.Process.Quick.CallEffect where


import System.Directory ( doesDirectoryExist, doesFileExist )
import System.Posix ( FileMode )
import System.Process.Quick.Prelude
import Text.Regex.TDFA
    ( RegexLike(matchTest),
      RegexMaker(makeRegexOpts),
      RegexOptions(defaultCompOpt),
      CompOption(multiline),
      ExecOption(ExecOption),
      Regex )

import Language.Haskell.TH.Syntax ( Lift )

data TimeReference
  = LaunchTime
  | BootTime
  | ExitTime
  | Now
  deriving (Show, Eq, Ord)

data FsPredicate
  = FsExists
  | DirStructMatches FsEffect
  | FsPathHasPerm FileMode -- ^ AND
  | FsTime Ordering TimeReference
  deriving (Show, Eq)

data FsEffect
  = FsPathPredicate FilePath [FsPredicate]
  | FsNot FsEffect
  | FsAnd [FsEffect]
  | FsOr [FsEffect]
  deriving (Show, Eq)

data Mismatch a
  = Mismatch
    { expected :: a
    , got ::a
    } deriving (Show, Eq)

data CallEffect
  = SleepFor Integer -- call lasts at least N microseconds
  | ExitCode (Mismatch ExitCode) -- expected exit code
  | FsEffect FsEffect
  | OrCe [CallEffect]
  | AndCe [CallEffect]
  | NotCe [CallEffect]
  | StdOutputCe { rx :: String, output :: String }
  | StdErrorCe { rx :: String, output :: String }
  deriving (Show, Eq)

-- move to module for orphan instances
deriving instance Lift ExitCode
deriving instance Data ExitCode

data CsExecReport
  = CsExecReport
    { exitCode :: ExitCode
    , stdErr :: String
    , stdOut :: String
    , execTime :: NominalDiffTime
    , processDir :: FilePath
    } deriving (Show, Eq)

class CallSpecOutcomeCheck c where
  -- | call after callSpec in the same directory
  check :: MonadIO m => CsExecReport -> c -> m [CallEffect]

data OutcomeChecker
  = ExitCodeEqualTo ExitCode
  | StdErrMatches String
  | StdOutMatches String
  | FileCreated FilePath
  | DirCreated FilePath
  deriving (Show, Eq, Data, Generic, Lift)
  -- | ConcatOutcomeChecker [OutcomeChecker]
  -- | BothOutcomeChecker OutcomeChecker OutcomeChecker

parseRx :: String -> Regex
parseRx = makeRegexOpts defaultCompOpt { multiline = False } (ExecOption False)

instance CallSpecOutcomeCheck OutcomeChecker where
  check cser | $(tr "/cser") True = \case
    ExitCodeEqualTo eec
      | eec == exitCode cser -> pure []
      | otherwise -> pure [ExitCode . Mismatch eec $ exitCode cser]
    StdErrMatches rx
      | parseRx rx `matchTest` stdErr cser -> pure []
      | otherwise -> pure [StdErrorCe rx $ stdErr cser]
    StdOutMatches rx
      | parseRx rx `matchTest` stdOut cser -> pure []
      | otherwise -> pure [StdOutputCe rx $ stdOut cser]
    FileCreated dp ->
      liftIO (doesFileExist dp) >>= \case
        True -> pure []
        False -> pure [FsEffect $ FsPathPredicate dp [FsExists]]
    DirCreated dp ->
      liftIO (doesDirectoryExist dp) >>= \case
        True -> pure []
        False -> pure [FsEffect $ FsPathPredicate dp [FsExists]]
