{-# LANGUAGE NoOverloadedStrings #-}
module SandBoxEffect where

import CallSpecs.CpOne ()
import CallSpecs.CpManyToDir ()
import CallSpecs.Date
import CallSpecs.FindCases ()
import CallSpecs.GitInit ()
import CallSpecs.GitInitExit1 ()
import System.Process.Quick
import System.Process.Quick.Prelude

main :: IO ()
main = do
  $(discoverAndVerifyCallSpecs (fromList [SandboxValidate]) 1)
  callProcess $ Date $$(refineTH "+%Y")
