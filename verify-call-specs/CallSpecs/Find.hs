-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Find where

import CallSpecs.Find.Type
import System.Process.Quick
import System.Process.Quick.Prelude hiding (NonEmpty, Type)

type DirPath = Refined FsPath String

$(genCallSpec
  [TrailingHelpValidate]
  "find"
  (ConstArg "-H" .*. VarArg @DirPath "path" .*. KeyArg @NodeType "-type" .*. HNil))
