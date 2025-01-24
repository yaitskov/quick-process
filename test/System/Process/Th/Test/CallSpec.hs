{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module System.Process.Th.Test.CallSpec where

import Data.HList as HL
import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec

type VarStrArg = VarArg String

$(genCallSpec "/bin/mkdir7" (VarArg @String "dn71" .*. VarArg @String "dn72" .*. HNil))

prop_BinMkdir7_name :: BinMkdir7 -> Bool
prop_BinMkdir7_name cs = programName cs == "/bin/mkdir7"

prop_BinMkdir7_args :: BinMkdir7 -> Bool
prop_BinMkdir7_args cs = length (programArgs cs) == 2

$(genCallSpec "/b/mkdir6" (ConstArg "-p" .*. VarArg @(Maybe Int) "dn6" .*. HNil))
$(genCallSpec "/b/mkdir5" (VarArg @(Maybe Int) "dn5" .*. HNil))
$(genCallSpec "/b/mkdir4" (VarArg @(Either Bool Int) "dn4" .*. HNil))
$(genCallSpec "/b/mkdir" (VarArg @Bool "dn2" .*. HNil))
$(genCallSpec "rm" (ConstArg "--version" .*. HNil))

prop_Rm_name :: Rm -> Bool
prop_Rm_name cs = programName cs == "rm"

prop_Rm_args :: Rm -> Bool
prop_Rm_args cs = programArgs cs == [ "--version" ]

$(genCallSpec "/bi/mkdir" (VarArg @String "dn" .*. HNil))

$(genCallSpec "/bin/mkdir" ((VarArg "dirname" :: VarStrArg) .*. HNil))

$(genCallSpec "mkdir" (ConstArg "--help" .*. HNil))


$(genCallSpec "rmdir" (ConstArg "-v" .*. ConstArg "--help" .*. HNil))
