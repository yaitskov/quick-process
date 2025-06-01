module System.Process.Quick.Test.Th where

import System.Process.Quick.Test.Prelude

import Refined
test_tree :: TestTree
test_tree =
  testGroup
    "hello"
    [ testCase "hello" (assertEqual "1 == 2 - 1" (1 :: Int) (2 - 1))

    ]

-- hMap (Fun show :: Fun Show (String)) (HCons () (HCons True HNil))
-- hFoldr (Fun show :: Fun Show (String)) [] (HCons () (HCons True HNil))

{-
class HFoldr f v (l :: [*]) r where
    hFoldr :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr f v '[] v' where
    hFoldr       _ v _   = v

instance (ApplyAB f (e, r) r', HFoldr f v l r)
    => HFoldr f v (e ': l) r' where
    hFoldr f v (HCons x l)    = applyAB f (x, hFoldr f v l :: r)

-}
prop_refined_less_than_5 :: Refined (LessThan 5) Int -> Bool
prop_refined_less_than_5 x = unrefine x < 5
