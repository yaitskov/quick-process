module System.Process.Quick.CallSpec.Type where

import System.Process.Quick.Prelude
import Language.Haskell.TH.Syntax

-- | DC definition order defines validation order
data VerificationMethod
  = TrailingHelpValidate
  | SandboxValidate
  deriving (Show, Ord, Eq, Typeable, Data, Generic, Lift)

class (Arbitrary cs, Data cs) => CallSpec cs where
  programName :: Proxy cs -> String
  programArgs :: cs -> [String]
  verificationMethods :: Proxy cs -> [VerificationMethod]
