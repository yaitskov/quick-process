
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Array-like access to HLists.
 -}

module Data.HList.HArray where

import Data.HList.FakePrelude
import Data.HList.HList


-- --------------------------------------------------------------------------
-- * Lookup

class HLookupByHNat (n :: HNat) (l :: [*]) where
  type HLookupByHNatR (n :: HNat) (l :: [*]) :: *
  hLookupByHNat :: Proxy n -> HList l -> HLookupByHNatR n l

instance HLookupByHNat HZero (e ': l) where
  type HLookupByHNatR HZero (e ': l) = e
  hLookupByHNat _ (HCons e _)        = e

instance HLookupByHNat n l => HLookupByHNat (HSucc n) (e ': l) where
  type HLookupByHNatR (HSucc n) (e ': l) = HLookupByHNatR n l
  hLookupByHNat n (HCons _ l) = hLookupByHNat (hPred n) l


-- --------------------------------------------------------------------------
-- * Delete

class HDeleteAtHNat (n :: HNat) (l :: [*]) where
  type HDeleteAtHNatR (n :: HNat) (l :: [*]) :: [*]
  hDeleteAtHNat :: Proxy n -> HList l -> HList (HDeleteAtHNatR n l)

instance HDeleteAtHNat HZero (e ': l) where
  type HDeleteAtHNatR  HZero (e ': l) = l
  hDeleteAtHNat _ (HCons _ l)         = l

instance HDeleteAtHNat n l => HDeleteAtHNat (HSucc n) (e ': l) where
  type HDeleteAtHNatR  (HSucc n) (e ': l) = e ': (HDeleteAtHNatR n l)
  hDeleteAtHNat n (HCons e l) = HCons e (hDeleteAtHNat (hPred n) l)


-- --------------------------------------------------------------------------
-- * Update
class HUpdateAtHNat' n e l l => HUpdateAtHNat n e l where
    hUpdateAtHNat :: Proxy n -> e -> HList l -> HList (HUpdateAtHNatR n e l)

instance HUpdateAtHNat' n e l l => HUpdateAtHNat n e l where
    hUpdateAtHNat = hUpdateAtHNat' (Proxy :: Proxy l)

class HUpdateAtHNat' (n :: HNat) e (l :: [*]) (l0 :: [*]) where
  type HUpdateAtHNatR (n :: HNat) e (l :: [*]) :: [*]
  hUpdateAtHNat' :: Proxy l0 -> Proxy n -> e -> HList l -> HList (HUpdateAtHNatR n e l)

instance HUpdateAtHNat' HZero e1 (e ': l) l0 where
  type HUpdateAtHNatR  HZero e1 (e ': l) = e1 ': l
  hUpdateAtHNat' _ _ e1 (HCons _ l)      = HCons e1 l

instance HUpdateAtHNat' n e1 l l0 => HUpdateAtHNat' (HSucc n) e1 (e ': l) l0 where
  type HUpdateAtHNatR  (HSucc n) e1 (e ': l) = e ': (HUpdateAtHNatR n e1 l)
  hUpdateAtHNat' l0 n e1 (HCons e l) = HCons e (hUpdateAtHNat' l0 (hPred n) e1 l)

instance Fail (HNatIndexTooLarge n HList l0) => HUpdateAtHNat' n e1 '[] l0 where
  type HUpdateAtHNatR n e1 '[] = '[]
  hUpdateAtHNat' _ _ _ = error "Data.HList.HArray.HUpdateAtHNat: Fail must have no instances"

-- --------------------------------------------------------------------------
-- * Projection

-- One way of implementing it:

hProjectByHNats' ns l = hMap (FHLookupByHNat l) ns

newtype FHLookupByHNat (l :: [*]) = FHLookupByHNat (HList l)

instance HLookupByHNat n l =>
    Apply (FHLookupByHNat l) (Proxy (n :: HNat)) where
  type ApplyR (FHLookupByHNat l) (Proxy n) = HLookupByHNatR n l
  apply (FHLookupByHNat l) n               = hLookupByHNat  n l

-- The drawback is that the list ns must be a constructed value.
-- We cannot lazily pattern-match on GADTs. Moreover, there are
-- repeated traversals of the HList l at run-time.

-- Here is a more optimal version with a better separation of
-- compile-time and run-time computation.
-- The list of labels to project is type-level only.
-- We treat this list of labels as a set -- that is, we will
-- ignore duplicates.
-- We traverse the HList l only once. The lookup in the list of
-- indices is compile-time only.
-- (In contrast, hProjectByHNats' does not ignore duplicates).
-- We unify hProjectByHNats and hProjectAwayByHNats in one
-- function, distinguished by the sel :: Bool in
-- FHUProj below. The operation hProjectByHNats corresponds
-- to sel = True (that is, elements of l whose indices are found in
-- ns are to be included in the result), whereas hProjectByHNats
-- corresponds to set = False.

hProjectByHNats (_ :: Proxy (ns :: [HNat])) l =
    hUnfold (FHUProj :: FHUProj True ns) (l,hZero)

data FHUProj (sel :: Bool) (ns :: [HNat]) = FHUProj

instance Apply (FHUProj sel ns) (HList '[],n) where
    type ApplyR (FHUProj sel ns) (HList '[],n) = HNothing
    apply _ _ = HNothing

instance (ch ~ Proxy (HBoolEQ sel (KMember n ns)),
          Apply (ch, FHUProj sel ns) (HList (e ': l),Proxy (n :: HNat))) =>
    Apply (FHUProj sel ns) (HList (e ': l),Proxy (n :: HNat)) where
    type ApplyR (FHUProj sel ns) (HList (e ': l),Proxy n) =
       ApplyR (Proxy (HBoolEQ sel (KMember n ns)), FHUProj sel ns)
              (HList (e ': l),Proxy n)
    apply fn s = apply (Proxy::ch,fn) s

instance Apply (Proxy True, FHUProj sel ns)
               (HList (e ': l),Proxy (n::HNat)) where
    type ApplyR (Proxy True, FHUProj sel ns) (HList (e ': l),Proxy n) =
        (HJust (e, (HList l,Proxy (HSucc n))))
    apply _ (HCons e l,n) = (HJust (e,(l,hSucc n)))

instance (Apply (FHUProj sel ns) (HList l, Proxy (HSucc n))) =>
    Apply (Proxy False, FHUProj sel ns)
          (HList (e ': l),Proxy (n::HNat)) where
    type ApplyR (Proxy False, FHUProj sel ns) (HList (e ': l),Proxy n) =
        ApplyR (FHUProj sel ns) (HList l, Proxy (HSucc n))
    apply (_,fn) (HCons _ l,n) = apply fn (l,hSucc n)


-- lifted member on naturals
type family KMember (n :: HNat) (ns :: [HNat]) :: Bool
type instance KMember n '[]       = False
type instance KMember n (n1 ': l) = HOr (HNatEq n n1) (KMember n l)

-- Useful abbreviations for complex types (which are inferred)
type HProjectByHNatsR (ns :: [HNat]) (l :: [*]) =
    HUnfold (FHUProj True ns) (HList l, Proxy 'HZero)

type HProjectByHNatsCtx ns l =
  (Apply (FHUProj True ns) (HList l, Proxy 'HZero),
      HUnfold' (FHUProj True ns)
       (HList l, Proxy 'HZero)
    )

-- * Complement of Projection

-- The naive approach is repeated deletion (which is a bit subtle
-- sine we need to adjust indices)
-- Instead, we compute the complement of indices to project away
-- to obtain the indices to project to, and then use hProjectByHNats.
-- Only the latter requires run-time computation. The rest
-- are done at compile-time only.

hProjectAwayByHNats (_ :: Proxy (ns :: [HNat])) l =
    hUnfold (FHUProj :: FHUProj False ns) (l,hZero)


-- Useful abbreviations for complex types (which are inferred)
type HProjectAwayByHNatsR (ns :: [HNat]) (l :: [*]) =
    HUnfold (FHUProj False ns) (HList l, Proxy 'HZero)

type HProjectAwayByHNatsCtx ns l =
  (Apply (FHUProj False ns) (HList l, Proxy 'HZero),
      HUnfold' (FHUProj False ns) (HList l, Proxy 'HZero)
  )

-- * Splitting
-- | Splitting an array according to indices

-- The following is not optimal; we'll optimize later if needed

hSplitByHNats ns l = (hProjectByHNats ns l,
                      hProjectAwayByHNats ns l)
{-
hSplitByHNats ns l = hSplitByHNats' ns (hFlag l)

class HNats ns => HSplitByHNats' ns l l' l'' | ns l -> l' l''
 where
  hSplitByHNats' :: ns -> l -> (l',l'')

instance HSplit l l' l''
      => HSplitByHNats' HNil l HNil l'
 where
  hSplitByHNats' HNil l = (HNil,l')
   where
    (l',_) = hSplit l

instance ( HLookupByHNat n l (e,b)
         , HUpdateAtHNat n (e,HFalse) l l'''
         , HSplitByHNats' ns l''' l' l''
         )
      =>   HSplitByHNats' (HCons n ns) l (HCons e l') l''
 where
  hSplitByHNats' (HCons n ns) l = (HCons e l',l'')
   where
    (e,_)    = hLookupByHNat  n l
    l'''     = hUpdateAtHNat  n (e,hFalse) l
    (l',l'') = hSplitByHNats' ns l'''
-}


{-

-- --------------------------------------------------------------------------
-- * Bounded lists

class HMaxLength l s
instance (HLength l s', HLt s' (HSucc s) HTrue) => HMaxLength l s

class HMinLength l s
instance (HLength l s', HLt s (HSucc s') HTrue) => HMinLength l s

class HSingleton l
instance HLength l (HSucc HZero) => HSingleton l

hSingle :: (HSingleton l, HHead l e) => l -> e
hSingle = hHead

-}
