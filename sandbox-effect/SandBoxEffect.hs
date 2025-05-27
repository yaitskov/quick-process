-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module SandBoxEffect where

import CallSpecs.Cp ()
import CallSpecs.FindCases ()
import System.Process.Th
import System.Process.Th.Prelude

main :: IO ()
main = $(discoverAndVerifyCallSpecs (fromList [TrailingHelpValidate, SandboxValidate]) 4)
