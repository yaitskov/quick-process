{-# LANGUAGE CPP #-}
#if (__GLASGOW_HASKELL__ < 709)
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic type equality predicate:
   The implementation based on overlapping instances
   The only place where overlapping instances are really used
   besides Label5

-}

module Data.HList.TypeEqO where

import Data.HList.FakePrelude

#if !NEW_TYPE_EQ
instance {-# OVERLAPPING #-} HEq x x True
instance {-# OVERLAPPABLE  #-} False ~ b => HEq x y b
-- instance TypeEq x y HFalse -- would violate functional dependency
#endif



class TupleType (t :: *) (b :: Bool) | t -> b
instance {-# OVERLAPPING #-} TupleType () True
instance {-# OVERLAPPING #-} TupleType (x,y) True
instance {-# OVERLAPPING #-} TupleType (x,y,z) True
-- Continue for a while
instance {-# OVERLAPPABLE #-} False ~ b => TupleType x b
-- instance TupleType x HFalse -- would violate functional dependency

-- overlaps an instance Show (Proxy t) for convenience
instance {-# OVERLAPPING #-} Show (Proxy True)  where show _ = "HTrue"
instance {-# OVERLAPPING #-} Show (Proxy False) where show _ = "HFalse"

instance {-# OVERLAPPING #-} HNat2Integral n => Show (Proxy (n :: HNat)) where
    show n = "H" ++ show (hNat2Integral n :: Integer)


instance {-# OVERLAPPABLE #-} hZero ~ HZero => ArityFwd f hZero
instance {-# OVERLAPPING #-} Arity f n => ArityFwd (x -> f) (HSucc n)


-- | All our keywords must be registered
class IsKeyFN (t :: *) (flag :: Bool) | t-> flag
-- | overlapping/fallback case
instance {-# OVERLAPPABLE #-} (False ~ flag) => IsKeyFN t flag

