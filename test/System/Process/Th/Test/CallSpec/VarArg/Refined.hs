{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module System.Process.Th.Test.CallSpec.VarArg.Refined where

import Refined
import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec


type M = Refined (SizeEqualTo 2) String

$(genCallSpec "rm" (VarArg @M "fileName" .*. HNil))
