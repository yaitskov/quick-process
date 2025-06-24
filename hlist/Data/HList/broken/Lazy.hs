{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances #-}

-- |
-- Lazy HLists: potentially infinite heterogeneous streams...
-- Based on the suggestion by Chung-chieh Shan, posted on the
-- Haskell mailing list on Sun Oct 29 13:51:58 EST 2006

module Data.HList.HLazy where

import Data.HList.FakePrelude
import Data.HList.HListPrelude

-- our dream is to write something similar to
fib = 1 : 1 : zipWith (+) fib (tail fib)


-- The denotation of the application of the `function' f to the
-- the argument x. The function is meant to be any Apply-able thing.
data Thunk f x = Thunk f x

class Force thunk result | thunk -> result where
    force :: thunk -> result

instance Force HNil HNil where
    force = id

instance Force (HCons a b) (HCons a b) where
    force = id

instance Apply f x r => Force (Thunk f x) r where
    force (Thunk f x) = apply f x


instance HList (Thunk g x)

-- Take the prefix of a stream, forcing thunks if needed.
-- We need this function for the sake of `printing' streams, at least

newtype HTake n = HTake n

instance Apply (HTake HZero) l HNil where
    apply _ _ = HNil
instance Apply (HTake (HSucc n)) HNil HNil where
    apply _ _ = HNil
instance Apply (HTake n) b b'
    => Apply (HTake (HSucc n)) (HCons a b) (HCons a b') where
    apply _ (HCons a b) =
        HCons a (apply (HTake (undefined::n)) b)
instance (Apply g x l, Apply (HTake (HSucc n)) l r)
    => Apply (HTake (HSucc n)) (Thunk g x) r where
    apply n (Thunk g x) = apply n (apply g x)

htake n l = apply (HTake n) l


htail :: (Force l (HCons a b)) => l -> b
htail l = b where HCons _ b = force l

-- First stream: all zeros
-- Note the mutual dependency between a term and an instance

data LZeros
lzeros = Thunk (undefined::LZeros) ()

instance Apply LZeros () (HCons HZero (Thunk LZeros ())) where
    apply _ _ = HCons hZero lzeros


tzeros = htake four lzeros
{-
*HLazy> tzeros
HCons HZero (HCons HZero (HCons HZero (HCons HZero HNil)))
-}


-- Second stream: of natural numbers

data LNats
lnats = Thunk (undefined::LNats)

instance HNat n => Apply LNats n (HCons n (Thunk LNats (HSucc n))) where
    apply _ n = HCons n (lnats (hSucc n))

tnats = htake five . htail $ lnats hZero
{-
 *Data.HList.HLazy> tnats
  HCons HSucc HZero (HCons HSucc (HSucc HZero) 
    (HCons HSucc (HSucc (HSucc HZero)) 
      (HCons HSucc (HSucc (HSucc (HSucc HZero))) 
        (HCons HSucc (HSucc (HSucc (HSucc (HSucc HZero)))) HNil))))
-}


-- Extend HFold, HMap and HZip for the Lazy lists.
-- We don't need to re-write these classes. We merely need to add
-- an instance that accounts for the Thunk

instance (Apply g x l, HFoldr f v l r)
    => HFoldr f v (Thunk g x) r where
  hFoldr f v (Thunk g x) = hFoldr f v (apply g x)


-- We make our map lazy: when we map over thunk, we make a thunk
newtype HMapC f = HMapC f

instance HMap f (Thunk g x) (Thunk (HMapC f) (Thunk g x)) where
  hMap f x = Thunk (HMapC f) x

instance (Force x l, HMap f l r) => Apply (HMapC f) x r where
    apply (HMapC f) l = hMap f (force l)



data Add = Add
instance HNat n => Apply Add (HZero,n) n
instance (HNat n, HNat m, Apply Add (n,m) r)
    => Apply Add (HSucc n, m) (HSucc r)

-- We obtain the list of ones by incrementing the list of zeros
data Incr = Incr
instance Apply Incr n (HSucc n)

lones = hMap Incr lzeros

tones = htake five lones
{-
 *Data.HList.HLazy> tones
  HCons HSucc HZero (HCons HSucc HZero 
   (HCons HSucc HZero (HCons HSucc HZero (HCons HSucc HZero HNil))))
-}

-- and the list of evens by doubling the list of naturals

data Twice = Twice
instance Apply Add (n,n) r => Apply Twice n r

levens = hMap Twice (lnats hZero)
tevens = htake five levens
{-
 *Data.HList.HLazy> tevens
 HCons HZero (HCons HSucc (HSucc HZero) 
    (HCons HSucc (HSucc (HSucc (HSucc HZero))) 
     (HCons HSucc (HSucc (HSucc (HSucc (HSucc (HSucc HZero)))))
      (HCons HSucc (HSucc (HSucc (HSucc (HSucc (HSucc 
              (HSucc (HSucc HZero))))))) HNil))))
-}



-- The class Zip in Zip.hs was made to handle lists of the same length
-- Here, we define a bit general one

class LZip l1 l2 l3 | l1 l2 -> l3 where
    lzip :: l1 -> l2 -> l3

instance LZip HNil l2 HNil where
    lzip _ _ = HNil

instance LZip (HCons a b) HNil HNil where
    lzip _ = id

instance LZip (Thunk a b) HNil HNil where
    lzip _ = id

instance LZip tx ty l
    => LZip (HCons hx tx) (HCons hy ty) (HCons (hx,hy) l) where
    lzip (HCons hx tx) (HCons hy ty) = HCons (hx,hy) (lzip tx ty)

data LZipC = LZipC
instance LZip (Thunk g x) (HCons a b)
              (Thunk LZipC ((Thunk g x),(HCons a b))) where
    lzip l1 l2 = Thunk LZipC (l1,l2)

instance LZip (HCons a b) (Thunk g x)
              (Thunk LZipC ((HCons a b),(Thunk g x))) where
    lzip l1 l2 = Thunk LZipC (l1,l2)

instance LZip (Thunk a b) (Thunk g x)
              (Thunk LZipC ((Thunk a b),(Thunk g x))) where
    lzip l1 l2 = Thunk LZipC (l1,l2)

instance (Force l1 r1, Force l2 r2, LZip r1 r2 r) =>
    Apply LZipC (l1,l2) r where
    apply _ (l1,l2) = lzip (force l1) (force l2)



-- All is ready for our Fibonacci. Of course the simplest way to write
-- Fibonacci is along the lines of LNats above: just carry the state in
-- the thunk. The way given below is more laborous -- but cooler.

data LFibs = LFibs

lfibs = HCons hone (HCons hone (Thunk LFibs ()))

lfibs' = hMap Add (lzip lfibs (htail lfibs))

instance Apply LFibs ()
    -- the latter type is the type of lfibs'
    -- I simply did ":t lfibs'" and cut and pasted the result from
    -- one Emacs buffer (GHCi prompt) to the other.
    (HCons (HSucc (HSucc HZero))
     (Thunk (HMapC Add)
      (Thunk LZipC
       (HCons (HSucc HZero) (Thunk LFibs ()), Thunk LFibs ()))))
 where
    apply _ _ = lfibs'


tfibs = htake seven lfibs

{-
  *Data.HList.HLazy> tfibs
  HCons HSucc HZero 
   (HCons HSucc HZero 
    (HCons HSucc (HSucc HZero) 
      (HCons HSucc (HSucc (HSucc HZero))
        (HCons HSucc (HSucc (HSucc (HSucc (HSucc HZero))))
          (HCons HSucc (HSucc (HSucc (HSucc (HSucc 
                              (HSucc (HSucc (HSucc HZero)))))))
            (HCons HSucc (HSucc (HSucc (HSucc (HSucc (HSucc 
                         (HSucc (HSucc (HSucc (HSucc (HSucc 
                         (HSucc (HSucc HZero)))))))))))) HNil))))))
-}



hone = hSucc hZero
four :: HSucc (HSucc (HSucc (HSucc HZero)))     -- A few sample numbers
four = undefined

-- five :: HSucc (HSucc (HSucc (HSucc (HSucc HZero)))f)our = undefined
five = hSucc four
seven = hSucc $ hSucc $ five


-- We could also use something like the following
newtype LFibs' = LFibs' (HCons (HSucc HZero) (Thunk LFibs' ()))

-- however, the Apply instance for that would be just the same as above,
-- so there is no much gain...

