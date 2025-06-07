module System.Process.Quick.CallSpec.Verify.Type where

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
