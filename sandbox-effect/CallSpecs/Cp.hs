-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Cp where


import System.Process.Th
import System.Process.Th.Prelude


$(genCallSpec "cp" (VarArg @(Refined (InFile "*") String) "source" .*. VarArg @(Refined (OutFile "*") String) "destination" .*. HNil))
