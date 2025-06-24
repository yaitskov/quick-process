{-# LANGUAGE CPP #-}
{- | Description: sorting

Benchmarks for these functions can be found at
<http://code.haskell.org/~aavogt/HList-nodup/Run.html>.

See <Data-HList-CommonMain.html#v:hSort>
for the public interface.

-}
module Data.HList.HSort where

import Data.HList.HList
import Data.HList.FakePrelude
import Data.HList.Label3

#if __GLASGOW_HASKELL__ > 707
import GHC.TypeLits (type (<=?), CmpSymbol)
-- | only in ghc >= 7.7
instance ((x <=? y) ~ b) => HEqBy HLeFn x y b
-- | only in ghc >= 7.7

{- |

>>> let b1 = Proxy :: HEqBy HLeFn "x" "y" b => Proxy b
>>> :t b1
b1 :: Proxy 'True

>>> let b2 = Proxy :: HEqBy HLeFn "x" "x" b => Proxy b
>>> :t b2
b2 :: Proxy 'True

>>> let b3 = Proxy :: HEqBy HLeFn "y" "x" b => Proxy b
>>> :t b3
b3 :: Proxy 'False

-}
instance (HEq (CmpSymbol x y) GT nb, HNot nb ~ b) => HEqBy HLeFn x y b
#endif

-- | the \"standard\" '<=' for types. Reuses 'HEqBy'
--
-- Note that ghc-7.6 is missing instances for Symbol and Nat, so that
-- sorting only works 'HNat' (as used by "Data.HList.Label3").
data HLeFn

instance HEqByFn HLeFn

instance (HLe x y ~ b) => HEqBy HLeFn x y b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Tagged x v) (Tagged y w) b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Label x) (Label y) b
instance HEqBy HLeFn x y b => HEqBy HLeFn (Proxy x) (Proxy y) b

-- | Data.HList.Label3 labels can only be compared if they belong
-- to the same namespace.
instance (HEqBy HLeFn n m b, ns ~ ns')
     => HEqBy HLeFn (Lbl n ns desc) (Lbl m ns' desc') b


-- | analogous to 'Data.Ord.Down'
data HDown a
instance HEqByFn a => HEqByFn (HDown a)
instance HEqBy f y x b => HEqBy (HDown f) x y b

-- | The HEqBy instances for @HNeq HLeFn@ gives '<'
data HNeq le
instance HEqByFn a => HEqByFn (HNeq a)
instance (HEqBy le y x b1, HNot b1 ~ b2) => HEqBy (HNeq le) x y b2

{- | @HIsAscList le xs b@ is analogous to

> b = all (\(x,y) -> x `le` y) (xs `zip` tail xs)

-}
class HEqByFn le => HIsAscList le (xs :: [*]) (b :: Bool) | le xs -> b

instance HEqByFn le => HIsAscList le '[x] True
instance HEqByFn le => HIsAscList le '[] True
instance (HEqBy le x y b1,
#if __GLASGOW_HASKELL__ > 906
         HEqByFn le,
#endif
         HIsAscList le (y ': ys) b2,
         HAnd b1 b2 ~ b3)  => HIsAscList le (x ': y ': ys) b3


-- | quick sort with a special case for sorted lists
class (SameLength a b, HEqByFn le) => HSortBy le (a :: [*]) (b :: [*]) | le a -> b where
    hSortBy :: Proxy le -> HList a -> HList b

type HSort x y = HSortBy HLeFn x y

hSort :: HSort x y => HList x -> HList y
hSort xs = hSortBy (Proxy :: Proxy HLeFn) xs

instance (SameLength a b,
          HIsAscList le a ok,
          HSortBy1 ok le a b,
          HEqByFn le) => HSortBy le a b where
    hSortBy = hSortBy1 (Proxy :: Proxy ok)

instance HSortBy1 True le a a where
    hSortBy1 _ _ a = a -- already sorted

instance HQSortBy le a b => HSortBy1 False le a b where
    hSortBy1 _ = hQSortBy

class HSortBy1 ok le (a :: [*]) (b :: [*]) | ok le a -> b where
    hSortBy1 :: Proxy ok -> Proxy le -> HList a -> HList b

-- * Merge Sort

{- | HMSortBy is roughly a transcription of this merge sort

> msort [] = []
> msort [x] = [x]
> msort [x,y] = hSort2 x y
> msort xs = case splitAt (length xs `div` 2) xs of
>              (a,b) -> msort a `merge` msort b

> hSort2 x y
>     | x <= y    = [x,y]
>     | otherwise = [y,x]

> merge (x : xs) (y : ys)
>   | x > y     = y : merge (x : xs) ys
>   | otherwise = x : merge xs (y : ys)

-}
class HEqByFn le => HMSortBy le (a :: [*]) (b :: [*]) | le a -> b where
    hMSortBy :: Proxy le -> HList a -> HList b


instance HEqByFn le => HMSortBy le '[] '[] where hMSortBy _ x = x
instance HEqByFn le => HMSortBy le '[x] '[x] where hMSortBy _ x = x
instance (HSort2 b x y ab, HEqBy le x y b, HEqByFn le) =>
    HMSortBy le '[x,y] ab where
      hMSortBy _ (a `HCons` b `HCons` HNil) = hSort2 (Proxy :: Proxy b) a b

class HSort2 b x y ab | b x y -> ab where
    hSort2 :: Proxy b -> x -> y -> HList ab

instance HSort2 True x y '[x,y] where
    hSort2 _ x y = x `HCons` y `HCons` HNil

instance HSort2 False x y '[y,x] where
    hSort2 _ x y = y `HCons` x `HCons` HNil

instance (HMerge le xs' ys' sorted,
#if __GLASGOW_HASKELL__ > 906
          HEqByFn le,
#endif
          HMSortBy le ys ys',
          HMSortBy le xs xs',
          HLengthEq (a ': b ': c ': cs) n2,
          HDiv2 n2 ~ n,
          HSplitAt n (a ': b ': c ': cs) xs ys)
  => HMSortBy le (a ': b ': c ': cs) sorted where
  hMSortBy le abbs = case hSplitAt (Proxy :: Proxy n) abbs of
      (xs, ys) -> hMerge le (hMSortBy le xs) (hMSortBy le ys)


class HMerge le x y xy | le x y -> xy where
    hMerge :: Proxy le -> HList x -> HList y -> HList xy

instance HMerge le '[] '[] '[] where hMerge _ _ _ = HNil
instance HMerge le (x ': xs) '[] (x ': xs) where hMerge _ x _ = x
instance HMerge le '[] (x ': xs) (x ': xs) where hMerge _ _ x = x

instance (HEqBy le x y b,
          HMerge1 b (x ': xs) (y ': ys) (l ': ls) hhs,
          HMerge le ls hhs srt)
    => HMerge le (x ': xs) (y ': ys) (l ': srt) where
  hMerge le xxs yys = case hMerge1 (Proxy :: Proxy b) xxs yys of
        (HCons l ls, hhs) -> l `HCons` hMerge le ls hhs

type HMerge1 b x y min max = (HCond b (HList x) (HList y) (HList min),
                              HCond b (HList y) (HList x) (HList max))
hMerge1 b x y = (hCond b x y, hCond b y x)

-- * Quick sort
{- | HQSortBy is this algorithm

> qsort (x : xs @ (_ : _)) = case partition (<= x) xs of
>                  (le, gt) -> qsort le ++ x : qsort gt
> qsort xs = xs

on random inputs that are not pathological (ie. not already sorted or reverse
sorted) this turns out to be faster than HMSortBy, so it is used by default.

-}
class HQSortBy le (a :: [*]) (b :: [*]) | le a -> b where
    hQSortBy :: Proxy le -> HList a -> HList b

instance HQSortBy le '[] '[] where hQSortBy _ x = x
instance HQSortBy le '[x] '[x] where hQSortBy _ x = x
instance (HPartitionEq le a (b ': bs) bGeq bLt,
        HQSortBy le bLt  sortedLt,
        HQSortBy le bGeq sortedGeq,
        HAppendListR sortedLt (a ': sortedGeq) ~ sorted,
        HAppendList sortedLt (a ': sortedGeq)) =>
    HQSortBy le (a ': b ': bs) sorted where
    hQSortBy le (a `HCons` xs) = case hPartitionEq le (Proxy :: Proxy a) xs of
                      (g,l) -> hQSortBy le l `hAppendList` (a `HCons` hQSortBy le g)




-- * More efficient HRLabelSet / HLabelSet
{- | Provided the labels involved have an appropriate instance of HEqByFn,
it would be possible to use the following definitions:

> type HRLabelSet = HSet
> type HLabelSet  = HSet

-}
class HEqByFn lt => HSetBy lt (ps :: [*])
instance (HEqByFn lt, HSortBy lt ps ps', HAscList lt ps') => HSetBy lt ps

class HSetBy (HNeq HLeFn) ps => HSet (ps :: [*])
instance HSetBy (HNeq HLeFn) ps => HSet ps

{- |

>>> let xx = Proxy :: HIsSet [Label "x", Label "x"] b => Proxy b
>>> :t xx
xx :: Proxy 'False

>>> let xy = Proxy :: HIsSet [Label "x", Label "y"] b => Proxy b
>>> :t xy
xy :: Proxy 'True

-}
class HIsSet (ps :: [*]) (b :: Bool) | ps -> b
instance HIsSetBy (HNeq HLeFn) ps b => HIsSet ps b

class HEqByFn lt => HIsSetBy lt (ps :: [*]) (b :: Bool) | lt ps -> b
instance (HEqByFn lt, HSortBy lt ps ps', HIsAscList lt ps' b) => HIsSetBy lt ps b


-- | @HAscList le xs@ confirms that xs is in ascending order,
-- and reports which element is duplicated otherwise.
class HEqByFn le => HAscList le (ps :: [*])

instance (HEqByFn le, HAscList0 le ps ps) => HAscList le ps

class HEqByFn le => HAscList0 le (ps :: [*]) (ps0 :: [*])

class HEqByFn le => HAscList1 le (b :: Bool) (ps :: [*]) (ps0 :: [*])
instance ( HAscList1 le b (y ': ys) ps0, HEqBy le x y b
#if __GLASGOW_HASKELL__ > 906
         , HEqByFn le
#endif
         )
  => HAscList0 le (x ': y ': ys) ps0
instance HEqByFn le => HAscList0 le '[] ps0
instance HEqByFn le => HAscList0 le '[x] ps0

instance ( Fail '("Duplicated element", y, "using le", le, "in", ys0), HEqByFn le )
    => HAscList1 le False (y ': ys) ys0
instance HAscList0 le ys ys0 => HAscList1 le True ys ys0

{- $setup

>>> import Data.HList.TypeEqO

-}
