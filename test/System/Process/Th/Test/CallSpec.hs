{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module System.Process.Th.Test.CallSpec where

import Data.HList as HL
import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec

type VarStrArg = VarArg String

$(genProgArgsRender "/b/mkdir5" (VarArg @(Maybe Int) "dn5" .*. HNil))
$(genProgArgsRender "/b/mkdir4" (VarArg @(Either Bool Int) "dn4" .*. HNil))
$(genProgArgsRender "/b/mkdir" (VarArg @Bool "dn2" .*. HNil))
$(genProgArgsRender "/b/mkdir2" (VarArg @Bool "dn3" .*. HNil))
$(genProgArgsRender "/bi/mkdir" (VarArg @String "dn" .*. HNil))

$(genProgArgsRender "/bin/mkdir" ((VarArg "dirname" :: VarStrArg) .*. HNil))

$(genProgArgsRender "mkdir" (ConstArg "--help" .*. HNil))


$(genProgArgsRender "rmdir" (ConstArg "-v" .*. ConstArg "--help" .*. HNil))
