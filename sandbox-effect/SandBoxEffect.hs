-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module SandBoxEffect where

import CallSpecs.CpOne ()
import CallSpecs.CpManyToDir ()
import CallSpecs.FindCases ()
import System.Process.Th
import System.Process.Th.Prelude

main :: IO ()
main = $(discoverAndVerifyCallSpecs (fromList [SandboxValidate]) 1)
