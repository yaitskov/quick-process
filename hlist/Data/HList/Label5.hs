{-# LANGUAGE CPP #-}
#if (__GLASGOW_HASKELL__ < 709)
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
#if __GLASGOW_HASKELL__ > 906
{-# LANGUAGE LambdaCase #-}
#endif
{- |
   Description: labels are any instance of Typeable

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Yet another model of labels.

   This model allow us to use any type as label type.
   As a result, we need some generic instances.

   Also, type errors may be more confusing now.
-}

module Data.HList.Label5 where

import Data.Typeable
import Data.Char
import Data.HList.FakePrelude


-- | Equality on labels

-- instance TypeEq x y b => HEq x y b


-- | Show label
instance {-# OVERLAPPABLE #-} Typeable (x :: *) => ShowLabel x
 where
  showLabel _ = (\l -> case l of [] -> [] ; (x:xs) -> toLower x:xs)
            . reverse
            . takeWhile (not . (==) '.')
            . reverse
            . show
{-
            . tyConString
            . typeRepTyCon
-}
            . typeOf $ (error "Data.HList.Label5 has a strict typeOf" :: x)
