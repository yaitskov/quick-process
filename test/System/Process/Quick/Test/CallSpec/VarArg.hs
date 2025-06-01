{-# LANGUAGE TemplateHaskell #-}
module System.Process.Quick.Test.CallSpec.VarArg where

import System.Process.Quick.Test.Prelude
import System.Process.Quick.CallArgument
import System.Process.Quick.CallSpec


$(genCallSpec [TrailingHelpValidate] "mkdir" (VarArg @String "dirName" .*. HNil))

prop_BinMkdir_name :: Mkdir -> Bool
prop_BinMkdir_name cs = programName (pure cs) == "mkdir"

prop_BinMkdir_args :: Mkdir -> Bool
prop_BinMkdir_args cs = length (programArgs cs) == 1
