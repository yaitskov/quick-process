{-# LANGUAGE NoOverloadedStrings #-}
module SandBoxEffect where

import CallSpecs.CpOne ()
import CallSpecs.CpManyToDir ()
import CallSpecs.Date
import CallSpecs.FindCases ()
import System.Process.Th
import System.Process.Th.Prelude

main :: IO ()
main = do
  $(discoverAndVerifyCallSpecs (fromList [SandboxValidate]) 1)
  callProcess $ Date $$(refineTH "+%Y")
