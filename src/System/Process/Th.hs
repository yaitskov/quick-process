{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE MonoLocalBinds #-} -- elim warning
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Th
  ( module CA
  , module CS
  , module HL
  , module System.Process.Th
  ) where


import Data.Char
import Data.HList as HL
import Language.Haskell.TH as TH
import Refined
import System.Process qualified as SP
import System.Process.Th.Prelude
import System.Process.Th.CallSpec as CS
import System.Process.Th.CallArgument as CA
import Test.QuickCheck  (Arbitrary (..))
data LowerCase

instance Predicate LowerCase String where
  validate p value =
    if all isLower value
      then Nothing
      else throwRefineOtherException (typeRep p) "Not all chars are lower-case"

type LowerCaseString = Refined LowerCase String

mkDir :: String -> IO ()
mkDir dname = do
  putStrLn dname
  $( runQ [| \x -> SP.callProcess "mkdir" [x] |] ) dname
-- callProcess :: String -> [String] -> Q Exp
-- callProcess = $(do
--   pname <- newName "program"
--   pparams <- newName "params"
--   pure (LamE [(VarP pname) (VarP pparams)]
--     (AppE (AppE 'SP.callProcess ))


--     )
--   )
--   pure (AppE (AppE (VarE 'SP.callProcess) (VarE 'program)) (VarE 'param))
-- a = SP.callProcess
  -- pathContentEntries :: [(FilePath, ApiDoc)] <-
  --   runIO $

leftError :: Show a => Text -> Either a b -> b
leftError m = \case
  Right v -> v
  Left e -> error $ m <> "; due: " <> show e

instance {-# OVERLAPPING #-}
  (Arbitrary a, Typeable a, Predicate (SizeEqualTo n) [a], KnownNat n) =>
  Arbitrary (Refined (SizeEqualTo n) [a]) where
  arbitrary =
    leftError "Dead code" . refine <$>
    replicateM (fromIntegral $ natVal (Proxy @n)) arbitrary

  shrink = rights . map refine . shrink . unrefine
