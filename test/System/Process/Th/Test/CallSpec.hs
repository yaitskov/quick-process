{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module System.Process.Th.Test.CallSpec where

import Data.HList as HL
import Refined
import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import TH.Utilities qualified as TU
import Language.Haskell.TH
type VarStrArg = VarArg String


x :: String
x = $(stringE . show =<< TU.typeRepToType (typeRep (Proxy @(Refined (SizeEqualTo 2) String))))
