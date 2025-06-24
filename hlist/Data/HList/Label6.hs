{-# LANGUAGE CPP #-}
{- |
   Description : labels using promoted strings (Symbol)

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke


   Yet another model of labels.

   Labels are promoted Strings or Integers "GHC.TypeLits" inside the
   'Label'. Needs ghc7.6 or higher.

   See <Data-HList-CommonMain.html#label6demo CommonMain#label6demo> for an example.

-}

module Data.HList.Label6 () where

import Data.HList.FakePrelude
import GHC.TypeLits
import Data.HList.HListPrelude

#if MIN_VERSION_base(4,7,0)
instance KnownSymbol x => ShowLabel (x :: Symbol) where
  showLabel _ =  symbolVal (Proxy :: Proxy x)
instance KnownNat x => ShowLabel (x :: Nat) where
  showLabel _ =  show $ natVal (Proxy :: Proxy x)
#else
instance SingI x => ShowLabel (x :: Symbol) where
  showLabel _ =  fromSing (sing :: Sing x)

instance SingI x => ShowLabel (x :: Nat) where
  showLabel _ =  show (fromSing (sing :: Sing x))
#endif



{- |

>>> let labelX = Label :: Label "x"
>>> let labelY = Label :: Label "y"
>>> let p = labelX .*. labelY .*. emptyProxy
>>> :t p
p :: Proxy '["x", "y"]

-}
instance HExtend (Label (y :: Symbol)) (Proxy (x ': xs :: [Symbol])) where
    type HExtendR (Label y) (Proxy (x ': xs)) = Proxy (y ': x ': xs)
    (.*.) _ _ = Proxy

instance HExtend (Label (y :: Nat)) (Proxy (x ': xs :: [Nat])) where
    type HExtendR (Label y) (Proxy (x ': xs)) = Proxy (y ': x ': xs)
    (.*.) _ _ = Proxy
