{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module System.Process.Th.Test.CallSpec where

import Data.HList as HL
import System.Process.Th.Test.Prelude
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec
-- import System.Process.Th.CallSpec (genProgArgsRender)

-- data YYY = YYY deriving (Show, Eq, Typeable)

-- test_Show_test :: Bool
-- test_Show_test =
--   $(do x <- isInstance (mkName "Show") [ ConT (mkName "YYY") ] ; pure (LitE (StringL (show x)))) == "True"

-- test_Typeable_test :: Bool
-- test_Typeable_test =
--   $(do x <- isInstance (mkName "Typeable") [ ConT (mkName "YYY") ] ; pure (LitE (StringL (show x)))) == "True"

-- $(genProgArgsRender "/b/mkdir4" ((VarArg @() "dn4") .*. HNil))
$(genProgArgsRender "/b/mkdir" ((VarArg @Bool "dn2") .*. HNil))
$(genProgArgsRender "/b/mkdir2" ((VarArg @Bool "dn3") .*. HNil))
$(genProgArgsRender "/bi/mkdir" ((VarArg @String "dn") .*. HNil))

$(genProgArgsRender "/bin/mkdir" (((VarArg "dirname") :: VarStrArg) .*. HNil))

$(genProgArgsRender "mkdir" (ConstArg "--help" .*. HNil))


$(genProgArgsRender "rmdir" (ConstArg "-v" .*. ConstArg "--help" .*. HNil))
