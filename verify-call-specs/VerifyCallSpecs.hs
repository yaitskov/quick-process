-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module VerifyCallSpecs where

import CallSpecs.Find ()
import System.Process.Th
import System.Process.Th.Prelude

main :: IO ()
main = $(discoverAndVerifyCallSpecs 7)
