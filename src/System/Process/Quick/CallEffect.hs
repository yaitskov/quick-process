module System.Process.Quick.CallEffect where

import System.Posix
import System.Process.Quick.Prelude
import Text.Regex.TDFA
import Prelude (Show (..))

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

data ViRex = ViRex ByteString Regex

instance Show ViRex where
  show (ViRex bs _) = Prelude.show bs

instance Eq ViRex where
  (ViRex a _) == (ViRex b _) = a == b

data OutMatcher
  = ExactMatching ByteString
  | WholeMatching ViRex -- read all input
  | LineMatching ViRex -- consume file line by line - at least one line match
  deriving (Show, Eq)

data CallEffect
  = SleepFor Integer -- call lasts at least N microseconds
  | ExitCode Int -- expected exit code
  | FsEffect FsEffect
  | OrCe [CallEffect]
  | AndCe [CallEffect]
  | NotCe [CallEffect]
  | StdOutputCe OutMatcher
  | StdErrorCe OutMatcher
  deriving (Show, Eq)

-- | instances are generated for types with CallSpec and Subcases
-- The class is introduced because,
-- expected effects don't have fields in a CallSpec record
class CallSpecEffect cse where
  -- | call after callSpec in the same directory
  unsatisfiedEffects :: MonadIO m => cse -> m [CallEffect]


newtype DirCreated = DirCreated FilePath deriving (Show, Eq, Data, Generic)

-- instance CallSpecEffect DirCreated where
--   unsatisfiedEffects (DirCreated dp) = do
