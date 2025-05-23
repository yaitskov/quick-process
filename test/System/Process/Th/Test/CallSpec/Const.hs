-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Process.Th.Test.CallSpec.Const where


import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec


$(genCallSpec [TrailingHelpValidate] "rm" (ConstArg "--version" .*. HNil))

prop_Rm_name :: Rm -> Bool
prop_Rm_name cs = programName (pure cs) == "rm"

prop_Rm_args :: Rm -> Bool
prop_Rm_args cs = programArgs cs == [ "--version" ]


$(genCallSpec [TrailingHelpValidate] "mkdir" (ConstArg "--help" .*. HNil))

prop_Mkdir_name :: Mkdir -> Property
prop_Mkdir_name cs = programName (pure cs) === "mkdir"

prop_Mkdir_args :: Mkdir -> Property
prop_Mkdir_args cs = programArgs cs === [ "--help" ]


$(genCallSpec [TrailingHelpValidate] "/bin/rmdir" (ConstArg "-x" .*. ConstArg "-v" .*. ConstArg "--help" .*. HNil))

prop_BinRmdir_name :: BinRmdir -> Property
prop_BinRmdir_name cs = programName (pure cs) === "/bin/rmdir"

prop_BinRmdir_args :: BinRmdir -> Property
prop_BinRmdir_args cs = programArgs cs === [ "-x", "-v", "--help" ]
