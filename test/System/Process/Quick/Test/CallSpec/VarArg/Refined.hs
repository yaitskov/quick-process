{-# LANGUAGE TemplateHaskell #-}
module System.Process.Quick.Test.CallSpec.VarArg.Refined where


import System.Process.Quick.Test.Prelude
import System.Process.Quick.CallArgument
import System.Process.Quick.CallSpec
import System.Process.Quick ()

type M = Refined (SizeEqualTo 2) String


$(genCallSpec [TrailingHelpValidate] "rm" (VarArg @M "fileName" .*. HNil))

prop_rm_filename_of_2_chars_args :: Rm -> Property
prop_rm_filename_of_2_chars_args cs = (length <$> (programArgs cs ^? ix 0)) === Just 2
