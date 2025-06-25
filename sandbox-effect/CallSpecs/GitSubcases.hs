-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitSubcases where

import CallSpecs.GitInit qualified as I
import Data.String qualified as S
import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "git"
  (   ConstArgs (S.words "--no-pager --no-replace-objects")
  .*. Subcases "GitSubCases"
      [ Subcase "Success"
        (   ConstArg "reset"
        .*. StdErrMatches "^$"
        .*. StdOutMatches "^$"
        .*. Init @I.Git
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
