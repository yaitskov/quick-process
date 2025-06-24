{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoTypeFamilies #-}
{- | Description: TIP functions needing different LANGUAGE extensions

While NoMonoLocalBinds could be enabled in TIP.hs, the ghc manual warns
"type inference becomes less predicatable if you do so. (Read the papers!)".
These definitions don't need type families, putting these definitions in
a separate module avoids that mess.

XXX these should be implemented in terms of 'HTuple' and 'tipyProject',
which means adding
-}
module Data.HList.TIPtuple where

import Data.HList.HOccurs

{- | project a TIP (or HList) into a tuple

@tipyTuple' x = ('hOccurs' x, hOccurs x)@

behaves similarly, except @tipyTuple@ excludes
the possibility of looking up the same element
twice, which allows inferring a concrete type
in more situations. For example

> (\x y z -> tipyTuple (x .*. y .*. emptyTIP) `asTypeOf` (x, z)) () 'x'

has type @Char -> ((), Char)@. tipyTuple' would
need a type annotation to decide whether the type
should be @Char -> ((), Char)@ or @() -> ((), ())@

-}
tipyTuple l = t (,) `asTypeOf` t (flip (,))
  where
  t f = case hOccursRest l of
     (x, ly) -> case hOccursRest ly of
         (y, _) -> f x y

tipyTuple3 l = t (,,)
          `asTypeOf` t (\a b c -> (b,c,a))
          `asTypeOf` t (\a b c -> (c,a,b))
  where
  t f = case hOccursRest l of
    (x, lyz) -> case hOccursRest lyz of
       (y, lz) -> case hOccursRest lz of
          (z, _) -> f x y z

tipyTuple4 l = t (,,,)
          `asTypeOf` t (\a b c d -> (b,c,d,a))
          `asTypeOf` t (\a b c d -> (c,d,a,b))
          `asTypeOf` t (\a b c d -> (d,a,b,c))
  where
  t f = case hOccursRest l of
    (a, lbcd) -> case hOccursRest lbcd of
       (b, lcd) -> case hOccursRest lcd of
          (c, ld) -> case hOccursRest ld of
             (d, _) -> f a b c d

tipyTuple5 l = t (,,,,)
          `asTypeOf` t (\a b c d e -> (b,c,d,e,a))
          `asTypeOf` t (\a b c d e -> (c,d,e,a,b))
          `asTypeOf` t (\a b c d e -> (d,e,a,b,c))
          `asTypeOf` t (\a b c d e -> (e,a,b,c,d))
  where
  t f = case hOccursRest l of
    (a, lbcde) -> case hOccursRest lbcde of
       (b, lcde) -> case hOccursRest lcde of
          (c, lde) -> case hOccursRest lde of
            (d, le) -> case hOccursRest le of
               (e, _) -> f a b c d e
