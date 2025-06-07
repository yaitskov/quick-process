-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitInitExit1 where

import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "git"
  (   ConstArg "initt"
  .*. ExitCodeEqualTo (ExitFailure 1)
  .*. StdErrMatches "is not a git command"
  .*. StdOutMatches "^$"
  -- .*. DirCreated ".git"
  .*. HNil
  )
 )
