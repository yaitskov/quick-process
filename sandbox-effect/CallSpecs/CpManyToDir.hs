-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.CpManyToDir where

import System.Process.Th
import System.Process.Th.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "cp"
  (   VarArg @(Refined (InFile "hs") (NeList FilePath)) "source"
  .*. VarArg @(Refined InDir FilePath) "destination"
  .*. HNil
  )
 )
