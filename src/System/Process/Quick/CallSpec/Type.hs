module System.Process.Quick.CallSpec.Type where

import System.Process.Quick.CallEffect ( OutcomeChecker )
import System.Process.Quick.Prelude
import Language.Haskell.TH.Syntax ( Lift )

-- | DC definition order defines validation order
data VerificationMethod
  = TrailingHelpValidate
  | SandboxValidate
  deriving (Show, Ord, Eq, Data, Bounded, Enum, Generic, Lift)

instance Pretty VerificationMethod where
  pretty = show

class (Show cs, Arbitrary cs, Data cs) => CallSpec cs where
  programName :: Proxy cs -> String
  programArgs :: cs -> [String]
  verificationMethods :: Proxy cs -> [VerificationMethod]
  outcomeCheckers :: cs -> [OutcomeChecker]
  initCallSpecs :: MonadIO m => cs -> m [CsBox]

-- avoid UndecidableInstances for init dependent CallSpecs
data CsBox = forall cs. CallSpec cs => CsBox { unCsBox :: cs }
