{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Process.Th.Predicate.LowerCase where

import Refined
import System.Process.Th.Prelude


data LowerCase

instance Predicate LowerCase String where
  validate p value =
    if all isLower value
      then Nothing
      else throwRefineOtherException (typeRep p) "Not all chars are lower-case"

type LowerCaseString = Refined LowerCase String


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
