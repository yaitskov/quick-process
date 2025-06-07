module System.Process.Quick.CallSpec.Type where

import System.Process.Quick.CallEffect ( OutcomeChecker )
import System.Process.Quick.Prelude
    ( Eq, Data, Ord, Show, Generic, String, Proxy, Arbitrary )
import Language.Haskell.TH.Syntax ( Lift )

-- | DC definition order defines validation order
data VerificationMethod
  = TrailingHelpValidate
  | SandboxValidate
  deriving (Show, Ord, Eq, Data, Generic, Lift)

class (Show cs, Arbitrary cs, Data cs) => CallSpec cs where
  programName :: Proxy cs -> String
  programArgs :: cs -> [String]
  verificationMethods :: Proxy cs -> [VerificationMethod]
  outcomeCheckers :: cs -> [OutcomeChecker]
