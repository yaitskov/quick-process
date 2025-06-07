-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitInit where

import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "git"
  (   ConstArg "init"
  .*. ExitCodeEqualTo ExitSuccess
  .*. StdErrMatches "^$"
  .*. StdOutMatches "^Initialized empty Git repository"
  .*. DirCreated ".git"
  .*. HNil
  )
 )
