{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of a type equality predicate. The given
   implementation only works for GHC. The specific coding here is only
   shown for completeness' sake. We actually favour the encoding from
   "Data.HList.TypeEqGeneric1" for its conciseness. The specific coding here
   does not rely on separate compilation (while TypeEqGeneric1.hs
   does), but on some other tricks.
-}

module Data.HList.TypeEqGeneric3 where

-- We make everything self-contained to show that separate compilation
-- is not needed. Also, we need a new class constraint for TypeEqBool,
-- (unless we again employ separate compilation in some ways) so
-- that instance selection of its generic instance within client code
-- of TypeEqBool does not issue problems with the instance
-- constraints.

import Data.HList.FakePrelude
import Data.HList.TypeCastGeneric3

-- Re-enabled for testing

typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined


{-----------------------------------------------------------------------------}

-- The actual encoding

-- TypeEq class itself is declared in FakePrelude
-- class TypeEq' () x y b => TypeEq x y b | x y -> b
class TypeEq' q x y b | q x y -> b
class TypeEq'' q x y b | q x y -> b
instance (HBool b, TypeEq' () x y b) => TypeEq x y b
instance TypeCast b HTrue => TypeEq' () x x b
instance TypeEq'' q x y b => TypeEq' q x y b
instance TypeEq'' () x y HFalse

{-----------------------------------------------------------------------------}
