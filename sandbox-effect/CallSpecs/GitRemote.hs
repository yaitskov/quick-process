-- {-# OPTIONS_GHC -ddump-splices #-}
module CallSpecs.GitRemote where

import CallSpecs.GitInit qualified as I
import System.Process.Quick

$(genCallSpec
  [SandboxValidate]
  "git"
  (   ConstArg "remote"
  .*. StdErrMatches "^$"
  .*. StdOutMatches "^$"
  .*. Init @I.Git
  .*. HNil
  )
 )
