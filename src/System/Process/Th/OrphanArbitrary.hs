{-# OPTIONS_GHC -Wno-orphans #-}
module System.Process.Th.OrphanArbitrary where

import System.Process.Th.Prelude

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) (arbitrary :: Gen a) (listOf (arbitrary :: Gen a))
