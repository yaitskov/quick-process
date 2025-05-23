{-# LANGUAGE TemplateHaskell #-}
module System.Process.Th.Test.CallSpec.VarArg where

import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec


$(genCallSpec [TrailingHelpValidate] "mkdir" (VarArg @String "dirName" .*. HNil))

prop_BinMkdir_name :: Mkdir -> Bool
prop_BinMkdir_name cs = programName (pure cs) == "mkdir"

prop_BinMkdir_args :: Mkdir -> Bool
prop_BinMkdir_args cs = length (programArgs cs) == 1
