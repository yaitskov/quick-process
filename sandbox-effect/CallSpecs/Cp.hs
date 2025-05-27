{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Cp where

import System.Process.Th
import System.Process.Th.Prelude


$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "cp"
  (   VarArg @(Refined (InFile "hs") String) "source"
  .*. VarArg @(Refined (OutFile "*") String) "destination"
  .*. HNil
  )
 )
