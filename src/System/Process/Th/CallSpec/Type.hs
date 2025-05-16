module System.Process.Th.CallSpec.Type where

import System.Process.Th.Prelude

class Arbitrary cs => CallSpec cs where
  programName :: cs -> String
  programArgs :: cs -> [String]
