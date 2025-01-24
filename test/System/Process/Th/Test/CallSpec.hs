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

csMkDir7 :: BinMkdir7
csMkDir7 = BinMkdir7 { dn71 = "a", dn72 = "b" }

prop_BinMkdir7_name :: Bool
prop_BinMkdir7_name = programName csMkDir7 == "/bin/mkdir7"

prop_BinMkdir7_args :: Bool
prop_BinMkdir7_args = programArgs csMkDir7 == [ "a", "b" ]

$(genCallSpec "/b/mkdir6" (ConstArg "-p" .*. VarArg @(Maybe Int) "dn6" .*. HNil))
$(genCallSpec "/b/mkdir5" (VarArg @(Maybe Int) "dn5" .*. HNil))
$(genCallSpec "/b/mkdir4" (VarArg @(Either Bool Int) "dn4" .*. HNil))
$(genCallSpec "/b/mkdir" (VarArg @Bool "dn2" .*. HNil))
$(genCallSpec "rm" (ConstArg "--version" .*. HNil))
$(genCallSpec "/bi/mkdir" (VarArg @String "dn" .*. HNil))

$(genCallSpec "/bin/mkdir" ((VarArg "dirname" :: VarStrArg) .*. HNil))

$(genCallSpec "mkdir" (ConstArg "--help" .*. HNil))


$(genCallSpec "rmdir" (ConstArg "-v" .*. ConstArg "--help" .*. HNil))
