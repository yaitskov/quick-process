
module System.Process.Quick.CallSpec.Verify.Type where

import Generic.Data
import Data.Typeable ( TypeRep )
import System.Process.Quick.CallEffect (CallEffect)
import System.Process.Quick.CallSpec (CallSpec)
import System.Process.Quick.Prelude hiding (show)
import Prelude

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
    , csExeTime :: !(Sum NominalDiffTime)
    } deriving (Show, Eq, Generic)

instance Ord CsPerf where
  CsPerf a b `compare` CsPerf c d = (a + b) `compare` (c + d)

instance Semigroup CsPerf where
  (<>) = gmappend

instance Monoid CsPerf where
  mempty = gmempty


type CsPerfT m = StateT (Map TypeRep CsPerf) m
