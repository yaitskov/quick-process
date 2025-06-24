module Curry where

import MainGhcGeneric1

class HUncurry f d c | f -> d c
 where
  hUncurry :: f -> d -> c

instance (FunType y b, HUncurry' b x y d c) => HUncurry (x -> y) d c
 where
  hUncurry (f::x->y) = hUncurry' (funType (undefined::y)) f

class HUncurry' b x y d c | b x y -> d c
 where
  hUncurry' :: b -> (x -> y) -> d -> c

instance HUncurry' HFalse x y (x,()) y
 where
  hUncurry' _ f (x,()) = f x

instance HUncurry y d c => HUncurry' HTrue x y (x,d) c
 where
  hUncurry' _ (f::x->y) (x,d) = hUncurry (f x) d

fun1 :: Int -> Int -> Int
fun1 = (+)

fun2 = hUncurry fun1

main = do
          print $ fun1 1 2
          print $ fun2 (1,(2,()))

