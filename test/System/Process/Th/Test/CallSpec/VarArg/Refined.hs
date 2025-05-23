{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Process.Th.Test.CallSpec.VarArg.Refined where


import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec
import System.Process.Th ()

type M = Refined (SizeEqualTo 2) String


$(genCallSpec [TrailingHelpValidate] "rm" (VarArg @M "fileName" .*. HNil))

prop_rm_filename_of_2_chars_args :: Rm -> Property
prop_rm_filename_of_2_chars_args cs = (length <$> (programArgs cs ^? ix 0)) === Just 2
