{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Date where

import System.Process.Quick
import System.Process.Quick.Prelude

$(genCallSpec
  [TrailingHelpValidate, SandboxValidate]
  "date"
  (   VarArg @(Refined (Regex "^[+][%][YmHdZ]$") String) "format"
  .*. HNil
  )
 )
