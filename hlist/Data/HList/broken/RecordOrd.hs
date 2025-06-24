{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- | records with Symbol labels that are sorted. Not enabled yet
-- because CmpSymbol comes with ghc>=7.8 and ghc-7.6 is not dropped yet
module Data.HList.RecordOrd where

import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.FakePrelude
import Data.Tagged

import GHC.TypeLits
import Data.Type.Equality

import Data.HList.Record

-- two-class version
class InsertOrd1 kv l l' | kv l -> l' where
    insertOrd1 :: kv -> HList l -> HList l'

class InsertOrd2 (b :: Ordering) kv l l' | b kv l -> l' where
    insertOrd2 :: Proxy b -> kv -> HList l -> HList l'


instance InsertOrd2 LT kv l (kv ': l) where
    insertOrd2 _ kv l = HCons kv l

instance (x ~ (l1 ': rest),
          y ~ (l1 ': rest'),
          InsertOrd1 kv rest rest') => InsertOrd2 GT kv x y where
    insertOrd2 _ kv (HCons l1 rest) = HCons l1 (insertOrd1 kv rest)
    insertOrd2 _ _ _ = error "Data.HList.RecordOrd: ghc bug"


instance Fail (DuplicatedLabel kv) => InsertOrd2 EQ kv x '[DuplicatedLabel kv] where
    insertOrd2 _ _ _ = error "Data.HList.RecordOrd: there must be no instances of Fail"


instance (  b ~ CmpSymbol k k',
            taggedkv ~ Tagged k v,
            taggedk'v' ~ Tagged k' v',
            InsertOrd2 b taggedkv (taggedk'v' ': kvs) kvs')
          => InsertOrd1 taggedkv (taggedk'v' ': kvs) kvs' where
    insertOrd1 kv kvs = insertOrd2 (Proxy :: Proxy b) kv kvs

instance InsertOrd1 x '[] '[x] where
    insertOrd1 kv _ = HCons kv HNil

data InsertOrd = InsertOrd
instance (x ~ (e, HList r),
          InsertOrd1 e r r',
          y ~ HList r'
        )=> ApplyAB InsertOrd x y where
  applyAB _ (e,r) = insertOrd1 e r

hSort' xs = hFoldr InsertOrd HNil xs


class HSorted (r :: [*])

instance HSorted '[]
instance HSorted '[x]
instance (HSorted (ty ': rest),
          tx ~ Tagged x vx,
          ty ~ Tagged y vy,
          CmpSymbol x y ~ LT) 
      => HSorted (tx ': ty ': rest)


{-

In principle this could be an appropriate proxy to use
for Variant: once the field ordering and number of fields
is fixed, as it would be for

_left = Label :: Label "left"
_right = Label :: Label "right"

(v,w) = case sortedProxyLen (hSucc (hSucc hZero)) of
              e -> (mkVariant _left 'x' e,
                    mkVariant _right (5 :: Int) e)

Then:

v :: (HasField' b1 "right" '[Tagged x a, Tagged y a1] Int,
      HasField' b "left" '[Tagged x a, Tagged y a1] Char,
      HFind2 b1 "right" '[y] n1, HFind2 b "left" '[y] n,
      HNat2Integral n1, HNat2Integral n, HEq "right" x b1,
      HEq "left" x b, GHC.TypeLits.CmpSymbol x y ~ 'LT) =>
     Variant '[Tagged x a, Tagged y a1]

Does not reduce to v :: Variant '[Tagged "left" Char, Tagged "right" Int]

If only ghc would backtrack after trying (x ~ "left") or (y ~ "right")
and see that only one will satisfy @CmpSymbol x y ~ LT@,


-}
sortedProxyLen :: (HSorted r,
                   SameLength' (HReplicateR n ()) r_,
                   HMapCxt (HFmap (Fun '[] ())) r r_) 
        => Proxy n -> Proxy r
sortedProxyLen _ = Proxy

class HSort xs xs' where
    hSort :: SameLength xs xs' => Record xs -> Record xs'

instance HFoldr InsertOrd (HList '[]) xs (HList xs') => HSort xs xs' where
    hSort (Record xs) = Record (hSort' xs)

{- two type family version
type family InsertOrd1 kv kvs where
  InsertOrd1 kv '[] = '[kv]
  InsertOrd1 (Tagged k v) (Tagged k' v' ': kvs) =
      InsertOrd2
        (CmpSymbol k k')
        (Tagged k v)
        (Tagged k' v' ': kvs)

type family InsertOrd2 (b :: Ordering) (kv :: *) (kvs :: [*]) where
  InsertOrd2 b x '[] = '[x]
  InsertOrd2 GT (Tagged k v) (Tagged k' v' ': kvs) =
          InsertOrd1 (Tagged k v) (Tagged k' v' ': kvs)
  InsertOrd2 LT kv kvs = kv ': kvs
  InsertOrd2 EQ kv kvs = Err (DuplicatedLabel kv) '[]

-- | Error messages: http://www.haskell.org/pipermail/haskell-cafe/2013-November/111549.html
type family Err (x::k) (a :: j) :: j where
   Err x a = Err x (Err "infinite loop to bring this to your attention: don't raise the context stack please" a)
-}


{-

-- Narrowing records
class (HRLabelSet a, HRLabelSet b) => Narrow a b where
    narrow :: Record a -> Record b

instance (HRLabelSet a,
          HRLabelSet b,
          HRLabelSet bOut,
          H2ProjectByLabels (RecordLabels b :: [k]) a b bOut)
      => Narrow a b where
    narrow a = case hProjectByLabels2 (Proxy :: Proxy (RecordLabels b :: [k])) a
      of (b :: Record b, _bOut :: Record bOut) -> b
      -}
