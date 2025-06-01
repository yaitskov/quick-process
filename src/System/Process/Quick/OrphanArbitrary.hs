{-# OPTIONS_GHC -Wno-orphans #-}
module System.Process.Quick.OrphanArbitrary where

import System.Process.Quick.Prelude

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) (arbitrary :: Gen a) (listOf (arbitrary :: Gen a))
