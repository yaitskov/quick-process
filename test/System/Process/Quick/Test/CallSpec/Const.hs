-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Process.Quick.Test.CallSpec.Const where


import System.Process.Quick.Test.Prelude
import System.Process.Quick.CallArgument
import System.Process.Quick.CallSpec


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


$(genCallSpec [TrailingHelpValidate] "rmdir" (ConstArg "-x" .*. ConstArg "-v" .*. ConstArg "--help" .*. HNil))

prop_Rmdir_name :: Rmdir -> Property
prop_Rmdir_name cs = programName (pure cs) === "rmdir"

prop_Rmdir_args :: Rmdir -> Property
prop_Rmdir_args cs = programArgs cs === [ "-x", "-v", "--help" ]
