
module System.Process.Quick.CallSpec.Verify.Type where

import Data.Multimap.Table ( Table )
import Data.Typeable ( TypeRep )
import Generic.Data ( gmappend, gmempty )
import Prelude (show)
import System.Process.Quick.CallEffect (CallEffect)
import System.Process.Quick.CallSpec.Type
import System.Process.Quick.Prelude hiding (show)

type FailureReport = Doc

data CallSpecViolation
  = HelpKeyIgnored
  | HelpKeyNotSupported FailureReport
  | ProgramNotFound FailureReport [FilePath]
  | HelpKeyExitNonZero FailureReport
  | ExceptionThrown SomeException
  | UnexpectedCallEffect [CallEffect]
  deriving (Show)

data CsViolationWithCtx
  = forall cs. CallSpec cs
  => CsViolationWithCtx
     { csContext :: cs
     , csViolation :: CallSpecViolation
     }

instance Show CsViolationWithCtx where
  show (CsViolationWithCtx cs csv) = "CsViolationWithCtx " <> show cs <> " " <> show csv

instance Exception CsViolationWithCtx


data CsPerf
  = CsPerf
    { csGenerationTime :: !(Sum NominalDiffTime)
    , csTotalTime :: !(Sum NominalDiffTime)
    , csExeTime :: !(Sum NominalDiffTime)
    } deriving (Show, Eq, Generic)

instance Ord CsPerf where
  compare = comparing (^. #csTotalTime)

instance Semigroup CsPerf where
  (<>) = gmappend

instance Monoid CsPerf where
  mempty = gmempty


type CsPerfT m = StateT (Table VerificationMethod TypeRep CsPerf) m
