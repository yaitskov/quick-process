-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitInit where

import Data.String qualified as S
import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "git"
  (   ConstArgs (S.words "-c init.defaultBranch=master init")
  .*. ExitCodeEqualTo ExitSuccess
  .*. StdErrMatches "^$"
  .*. StdOutMatches "^Initialized empty Git repository"
  .*. DirCreated ".git"
  .*. HNil
  )
 )
