-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitSubcases where

import Data.String qualified as S
import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "git"
  (   ConstArgs (S.words "-c init.defaultBranch=master")
  .*. Subcases "GitInit"
      [ Subcase "Success"
        (   ConstArg "init"
        .*. StdErrMatches "^$"
        .*. StdOutMatches "^Initialized empty Git repository"
        .*. DirCreated ".git"
        .*. HNil
        )
      , Subcase "Fail"
        (  ConstArg "initt"
        .*. ExitCodeEqualTo (ExitFailure 1)
        .*. StdErrMatches "is not a git command"
        .*. StdOutMatches "^$"
        .*. HNil
        )
      ]
  .*. HNil
  )
 )
