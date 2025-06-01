{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Date where

import System.Process.Th
import System.Process.Th.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "date"
  (   VarArg @(Refined (Regex "^[+][%][YmHdZ]$") String) "format"
  .*. HNil
  )
 )
