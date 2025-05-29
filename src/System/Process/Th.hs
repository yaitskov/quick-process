{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Th
  ( module M
  , module System.Process.Th
  ) where

import Data.Char
import Data.HList as M hiding (Arity)
import Language.Haskell.TH as M
import Refined as M
import System.Process as SP
import System.Process.Th.CallArgument as M
import System.Process.Th.CallSpec as M
import System.Process.Th.CallSpec.Subcases as M
import System.Process.Th.CallSpec.Verify as M
import System.Process.Th.Predicate as M
import System.Process.Th.Predicate.InDir as M
import System.Process.Th.Predicate.InFile as M
import System.Process.Th.Predicate.Regex as M
import System.Process.Th.Prelude

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
