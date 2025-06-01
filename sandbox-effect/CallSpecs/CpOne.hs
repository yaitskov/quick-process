-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.CpOne where

import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "cp"
  (   VarArg @(Refined (InFile "hs") FilePath) "source"
  .*. VarArg @(Refined (OutFile "*") FilePath) "destination"
  .*. HNil
  )
 )
