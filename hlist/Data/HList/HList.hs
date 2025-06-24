{-# LANGUAGE CPP #-}
{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Basic declarations for typeful heterogeneous lists.

 -}

module Data.HList.HList where

import Data.HList.FakePrelude
import Data.HList.HListPrelude

import Text.ParserCombinators.ReadP
import Data.List

import LensDefs

import Data.Array (Ix)

#if __GLASGOW_HASKELL__ <= 906
import Data.Semigroup
#endif

-- --------------------------------------------------------------------------
-- * Heterogeneous type sequences
{- $note

There are three sensible ways to define HLists:

@
data HList (l::[*]) where
    HNil  :: HList '[]
    HCons :: e -> HList l -> HList (e ': l)
@

This ensures that sequences can only be formed with Nil
and Cons. The argument to HList is a promoted lists (kind @[*]@),
which has a more attractive syntax.


Earlier versions of HList used an algebraic data type:

@
data HCons a b = HCons a b
data HNil = HNil
@

Disadvantages:

* values with types like @HCons Int Double@ to be created,
  which are nonsense to the functions in HList

* some recursive functions do not need a class with the GADT. For example:

  @
    hInit :: HListGADT (x ': xs) -> HListGADT (HInit (x ': xs))
    hInit (HCons x xs@(HCons _ _)) = HCons x (hInit xs)
    hInit (HCons _ HNil) = HNil

    type family HInit (xs :: [k]) :: [k]
  @

  but without the GADT, 'hInit' is written as in a class,
  which complicates inferred types


Advantages

* lazy pattern matches are allowed, so lazy pattern matching
  on a value @undefined :: HList [a,b,c]@ can create the
  spine of the list. 'hProxies' avoids the use of 'undefined',
  but a slightly more complicated class context has to be written
  or inferred.

* type inference is better if you want to directly pattern match
<http://stackoverflow.com/questions/19077037/is-there-any-deeper-type-theoretic-reason-ghc-cant-infer-this-type see stackoverflow post here>

* better pattern exhaustiveness checking (as of ghc-7.8)

* standalone deriving works

* Data.Coerce.coerce works because the parameters have role representational,
  not nominal as they are for the GADT and data family. Probably the GADT/type
  family actually do have a representational role:
  <http://stackoverflow.com/questions/24222552/does-this-gadt-actually-have-type-role-representational>



The data family version (currently used) gives the same type constructor
@HList :: [*] -> *@ as the GADT, while pattern matching behaves
like the algebraic data type. Furthermore, nonsense values like
@HCons 1 2 :: HCons Int Int@ cannot be written with the data family.

A variation on the data family version is

> data instance HList '[] = HNil
> newtype instance HList (x ': xs) = HCons1 (x, HList xs)
> pattern HCons x xs = HCons1 (x, xs)

This allows HList to have a nominal role, but on the other
hand the PatternSynonym is not supported with ghc-7.6 and
exhaustiveness checking is not as good (warnings for _ being
unmatched)

-}


data family HList (l::[*])

data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs

deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Ord (HList '[])
deriving instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

deriving instance Ix (HList '[])
deriving instance (Ix x, Ix (HList xs)) => Ix (HList (x ': xs))

deriving instance Bounded (HList '[])
deriving instance (Bounded x, Bounded (HList xs)) => Bounded (HList (x ': xs))


-- Enum cannot be derived


-- | creates a HList of Proxies

class HProxiesFD (xs :: [*]) pxs | pxs -> xs -- DropProxy pxs ~ xs
                      , xs -> pxs -- AddProxy xs ~ pxs
      where hProxies :: HList pxs

{- Ideally we could write:

> class DropProxy (AddProxy xs) ~ xs => HProxies xs where
>     hProxies :: HList (AddProxy xs)

See https://ghc.haskell.org/trac/ghc/ticket/10009 -}
type HProxies xs = HProxiesFD xs (AddProxy xs)


{- | Add 'Proxy' to a type

>>> let x = undefined :: HList (AddProxy [Char,Int])
>>> :t x
x :: HList '[Proxy Char, Proxy Int]


-}
type family AddProxy (xs :: k) :: k
type instance AddProxy '[] = '[]
type instance AddProxy (x ': xs) = AddProxy x ': AddProxy xs
type instance AddProxy (x :: *) = Proxy x

-- | inverse of 'AddProxy'
type family DropProxy (xs :: k) :: k
type instance DropProxy (x ': xs) = DropProxy x ': DropProxy xs
type instance DropProxy '[] = '[]
type instance DropProxy (Proxy x) = x

instance HProxiesFD '[] '[] where
    hProxies = HNil

instance (HProxiesFD xs pxs) => HProxiesFD (x ': xs) (Proxy x ': pxs) where
    hProxies = Proxy `HCons` hProxies



instance Show (HList '[]) where
    show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
    show (HCons x l) =
      case show l of
        'H':'[':s -> "H[" ++ show x ++ (if s == "]" then s else "," ++ s)
        s -> error $ "unreachable branch: " ++ show x ++ " " ++ s

instance Read (HList '[]) where
    readsPrec _ str = case stripPrefix "H[]" str of
                        Nothing -> []
                        Just rest -> [(HNil, rest)]

instance
   (HProxies l, Read e,
    HSequence ReadP (ReadP e ': readP_l) (e ': l),
    HMapCxt HList ReadElement (AddProxy l) readP_l)  =>
      Read (HList (e ': l)) where
  readsPrec _ = readP_to_S $ do
    _ <- string "H["
    l <- return (hProxies :: HList (AddProxy l))
    let parsers = readS_to_P reads `HCons` hMap ReadElement l
    hlist <- hSequence parsers
    _ <- string "]"
    return hlist


-- similar to ReadComponent used to define instance Read Record
data ReadElement = ReadElement

instance (y ~ ReadP x, Read x) => ApplyAB ReadElement (Proxy x) y where
    applyAB ReadElement _ = do
      _ <- string ","
      readS_to_P reads


infixr 2 `HCons`


-- --------------------------------------------------------------------------
-- * Basic list functions

-- | 'head'
hHead :: HList (e ': l) -> e
hHead (HCons x _) = x

-- | 'tail'
hTail :: HList (e ': l) -> HList l
hTail (HCons _ l) = l

-- | 'last'
hLast xs = hHead (hReverse_ xs)


class HInit xs where
    type HInitR xs :: [*]
    hInit :: HList xs -> HList (HInitR xs)

instance HInit '[x] where
    type HInitR '[x] = '[]
    hInit _ = HNil

instance HInit (b ': c) => HInit (a ': b ': c) where
    type HInitR (a ': b ': c) = a ': HInitR (b ': c)
    hInit (a `HCons` bc) = a `HCons` hInit bc


-- | Length, but see 'HLengthEq' instead
type family HLength (x :: [k]) :: HNat
type instance HLength '[] = HZero
type instance HLength (x ': xs) = HSucc (HLength xs)

hLength   :: HLengthEq l n => HList l -> Proxy n
hLength _ =  Proxy

-- ** Append
instance HExtend e (HList l) where
  type HExtendR e (HList l) = HList (e ': l)
  (.*.) = HCons

instance HAppendList l1 l2 => HAppend (HList l1) (HList l2) where
  hAppend = hAppendList
type instance HAppendR (HList l1) (HList l2) = HList (HAppendListR l1 l2)

type family HAppendListR (l1 :: [k]) (l2 :: [k]) :: [k]
type instance HAppendListR '[] l = l
type instance HAppendListR (e ': l) l' = e ': HAppendListR l l'


class HAppendList l1 l2 where
  -- | the same as 'hAppend'
  hAppendList :: HList l1 -> HList l2 -> HList (HAppendListR l1 l2)

instance HAppendList '[] l2 where
  hAppendList HNil l = l

instance HAppendList l l' => HAppendList (x ': l) l' where
  hAppendList (HCons x l) l' = HCons x (hAppendList l l')

-- --------------------------------------------------------------------------

-- ** Alternative append


-- | 'hAppend'' below is implemented using the same idea
append' :: [a] -> [a] -> [a]
append' l l' = foldr (:) l' l

-- | Alternative implementation of 'hAppend'. Demonstrates 'HFoldr'
hAppend' :: (HFoldr FHCons v l r) => HList l -> v -> r
hAppend' l l' = hFoldr FHCons l' l

data FHCons = FHCons

instance ( x ~ (e,HList l), y ~ (HList (e ': l))) => ApplyAB FHCons x y  where
    applyAB _ (e,l) = HCons e l


-- ** Historical append

{- $

The original HList code is included below. In both cases
we had to program the algorithm twice, at the term and the type levels.

[@The class HAppend@]

> class HAppend l l' l'' | l l' -> l''
>  where
>   hAppend :: l -> l' -> l''
>

[@The instance following the normal append@]

> instance HList l => HAppend HNil l l
>  where
>   hAppend HNil l = l
>
> instance (HList l, HAppend l l' l'')
>       => HAppend (HCons x l) l' (HCons x l'')
>  where
>   hAppend (HCons x l) l' = HCons x (hAppend l l')

-}

-- --------------------------------------------------------------------------
-- * Reversing HLists

-- Append the reversed l1 to l2
type family HRevAppR (l1 :: [k]) (l2 :: [k]) :: [k]
type instance HRevAppR '[] l = l
type instance HRevAppR (e ': l) l' = HRevAppR l (e ': l')


class HRevApp l1 l2 l3 | l1 l2 -> l3 where
    hRevApp :: HList l1 -> HList l2 -> HList l3

instance HRevApp '[] l2 l2 where
    hRevApp _ l = l

instance HRevApp l (x ': l') z => HRevApp (x ': l) l' z where
    hRevApp (HCons x l) l' = hRevApp l (HCons x l')



class HReverse xs sx | xs -> sx, sx -> xs where
    hReverse :: HList xs -> HList sx

instance (HRevApp xs '[] sx,
          HRevApp sx '[] xs) => HReverse xs sx where
    hReverse l = hRevApp l HNil

-- | a version of 'hReverse' that does not allow the type
-- information to flow backwards
hReverse_ l = hRevApp l HNil

-- --------------------------------------------------------------------------

--
-- * A nicer notation for lists
--


-- | List termination
hEnd :: HList l -> HList l
hEnd = id

{- ^
   Note:

        [@x :: HList a@] means: @forall a. x :: HList a@

        [@hEnd x@] means: @exists a. x :: HList a@
-}


-- |  Building lists

hBuild :: (HBuild' '[] r) => r
hBuild =  hBuild' HNil

class HBuild' l r where
    hBuild' :: HList l -> r

instance HReverse l l'
      => HBuild' l (HList l') where
  hBuild' l = hReverse l

instance HBuild' (a ': l) r
      => HBuild' l (a->r) where
  hBuild' l x = hBuild' (HCons x l)

-- ** examples
{- $examplesNote

The classes above allow the third (shortest) way to make a list
(containing a,b,c) in this case

> list = a `HCons` b `HCons` c `HCons` HNil
> list = a .*. b .*. c .*. HNil
> list = hEnd $ hBuild a b c

>>> let x = hBuild True in hEnd x
H[True]

>>> let x = hBuild True 'a' in hEnd x
H[True,'a']

>>> let x = hBuild True 'a' "ok" in hEnd x
H[True,'a',"ok"]

hBuild can also produce a Record, such that

> hBuild x y ^. from unlabeled

can also be produced using

@
'hEndR' $ hBuild x y
@

-}

-- *** historical
{- $hbuild the show instance has since changed, but these uses of
'hBuild'/'hEnd' still work

> HList> let x = hBuild True in hEnd x
> HCons True HNil

> HList> let x = hBuild True 'a' in hEnd x
> HCons True (HCons 'a' HNil)

> HList> let x = hBuild True 'a' "ok" in hEnd x
> HCons True (HCons 'a' (HCons "ok" HNil))

> HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5))
> HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))

> HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5)) == angus
> True

-}

-- --------------------------------------------------------------------------

-- * folds
-- ** foldr
-- $foldNote  Consume a heterogenous list.


class HFoldr f v (l :: [*]) r where
    hFoldr :: f -> v -> HList l -> r

instance (v ~ v') => HFoldr f v '[] v' where
    hFoldr       _ v _   = v

-- | uses 'ApplyAB' not 'Apply'
instance (ApplyAB f (e, r) r', HFoldr f v l r)
    => HFoldr f v (e ': l) r' where
    hFoldr f v (HCons x l)    = applyAB f (x, hFoldr f v l :: r)


class HScanr f z ls rs where
    hScanr :: f -> z -> HList ls -> HList rs

instance lz ~ '[z] => HScanr f z '[] lz where
    hScanr _ z _ = HCons z HNil

instance (ApplyAB f (x,r) s, HScanr f z xs (r ': rs),
          srrs ~ (s ': r ': rs)) => HScanr f z (x ': xs) srrs where
    hScanr f z (HCons x xs) =
        case hScanr f z xs :: HList (r ': rs) of
            HCons r rs -> (applyAB f (x,r) :: s) `HCons` r `HCons` rs

class HFoldr1 f (l :: [*]) r where
    hFoldr1 :: f -> HList l -> r

instance (v ~ v') => HFoldr1 f '[v] v' where
    hFoldr1      _ (HCons v _)  = v

-- | uses 'ApplyAB' not 'Apply'
instance (ApplyAB f (e, r) r', HFoldr1 f (e' ': l) r)
    => HFoldr1 f (e ': e' ': l) r' where
    hFoldr1 f (HCons x l)    = applyAB f (x, hFoldr1 f l :: r)


-- ** foldl

{- | like 'foldl'


>>> hFoldl (uncurry $ flip (:)) [] (1 `HCons` 2 `HCons` HNil)
[2,1]


-}
class HFoldl f (z :: *) xs (r :: *) where
    hFoldl :: f -> z -> HList xs -> r

instance (zx ~ (z,x), ApplyAB f zx z', HFoldl f z' xs r)
    => HFoldl f z (x ': xs) r where
    hFoldl f z (x `HCons` xs) = hFoldl f (applyAB f (z,x) :: z') xs

instance (z ~ z') => HFoldl f z '[] z' where
    hFoldl _ z _ = z





-- * unfolds

-- ** unfold
-- $unfoldNote Produce a heterogenous list. Uses the more limited
-- 'Apply' instead of 'App' since that's all that is needed for uses of this
-- function downstream. Those could in principle be re-written.

-- hUnfold :: (Apply p s, HUnfold' p s) => p -> s -> HList (HUnfold p s)
hUnfold p s = hUnfold' p (apply p s)

type HUnfold p s = HUnfoldR p (ApplyR p s)

type family HUnfoldR p res :: [*]
type instance HUnfoldR p HNothing = '[]
type instance HUnfoldR p (HJust (e,s)) = e ': HUnfoldR p (ApplyR p s)

type HUnfold' p res = HUnfoldFD p (ApplyR p res) (HUnfold p res)

class HUnfoldFD p res z | p res -> z where
    hUnfold' :: p -> res -> HList z

instance HUnfoldFD p HNothing '[] where
    hUnfold' _ _ = HNil

instance (Apply p s, HUnfoldFD p (ApplyR p s) z) => HUnfoldFD p (HJust (e,s)) (e ': z) where
    hUnfold' p (HJust (e,s)) = HCons e (hUnfold p s)


-- ** replicate

{- |

Sometimes the result type can fix the type of the
first argument:

>>> hReplicate Proxy () :: HList '[ (), (), () ]
H[(),(),()]

However, with HReplicate all elements must have the same type, so it may be
easier to use 'HList2List':

>>> list2HList (repeat 3) :: Maybe (HList [Int, Int, Int])
Just H[3,3,3]

-}
class HLengthEq es n => HReplicateFD (n :: HNat) e es
          | n e -> es, es -> n where
    hReplicate :: Proxy n -> e -> HList es

instance HReplicateFD HZero e '[] where
    hReplicate _ _ = HNil

instance (HReplicateFD n e es, e ~ e') => HReplicateFD (HSucc n) e (e' ': es) where
    hReplicate n e = e `HCons` hReplicate (hPred n) e

type HReplicate n e = HReplicateFD n e (HReplicateR n e)

-- | would be associated with 'HReplicate' except we want
-- it to work with `e` of any kind, not just `*` that you can
-- put into a HList. An \"inverse\" of 'HLength'
type family HReplicateR (n :: HNat) (e :: k) :: [k]
type instance HReplicateR HZero e = '[]
type instance HReplicateR (HSucc n) e = e ': HReplicateR n e

{- | HReplicate produces lists that can be converted to ordinary
lists

>>> let two = hSucc (hSucc hZero)
>>> let f = Fun' fromInteger :: Fun' Num Integer

>>> :t applyAB f
applyAB f :: Num b => Integer -> b

>>> hReplicateF two f 3
H[3,3]

>>> hReplicateF Proxy f 3 :: HList [Int, Double, Integer]
H[3,3.0,3]

-}
class HLengthEq r n => HReplicateF (n :: HNat) f z r | r -> n where
    hReplicateF :: HLengthEq r n => Proxy n -> f -> z -> HList r

instance HReplicateF HZero f z '[] where
    hReplicateF _ _ _ = HNil

instance (ApplyAB f z fz,
          HReplicateF n f z r')
  => HReplicateF (HSucc n) f z (fz ': r') where
    hReplicateF n f z = applyAB f z `HCons` hReplicateF (hPred n) f z

-- ** iterate
{- |

This function behaves like 'iterate', with an extra
argument to help figure out the result length

>>> let three = hSucc (hSucc (hSucc hZero))
>>> let f = Fun Just :: Fun '() Maybe

>>> :t applyAB f
applyAB f :: a -> Maybe a

f is applied to different types:

>>> hIterate three f ()
H[(),Just (),Just (Just ())]

It is also possible to specify the length later on,
as done with Prelude.'iterate'

>>> let take3 x | _ <- hLength x `asTypeOf` three = x
>>> take3 $ hIterate Proxy f ()
H[(),Just (),Just (Just ())]

-}
class HLengthEq r n => HIterate n f z r where
    hIterate :: HLengthEq r n => Proxy n -> f -> z -> HList r

instance HIterate HZero f z '[] where
    hIterate _ _ _ = HNil

instance (ApplyAB f z z',
      HIterate n f z' r',
      z ~ z_)
     => HIterate (HSucc n) f z (z_ ': r') where
    hIterate n f z = z `HCons` hIterate (hPred n) f (applyAB f z :: z')

-- * concat

{- |

Like 'concat' but for HLists of HLists.

Works in ghci... puzzling as what is different in doctest (it isn't
@-XExtendedDefaultRules@)

>>> let a = hEnd $ hBuild 1 2 3
>>> let b = hEnd $ hBuild 'a' "abc"
>>> hConcat $ hBuild a b
H[1,2,3,'a',"abc"]

-}
type HConcat xs = HConcatFD xs (HConcatR xs)

hConcat :: HConcat xs => HList xs -> HList (HConcatR xs)
hConcat x = hConcatFD x

type family HConcatR (a :: [*]) :: [*]
type instance HConcatR '[] = '[]
type instance HConcatR (x ': xs) = HAppendListR (UnHList x) (HConcatR xs)

type family UnHList a :: [*]
type instance UnHList (HList a) = a

-- for the benefit of ghc-7.10.1
class HConcatFD xxs xs | xxs -> xs
    where hConcatFD :: HList xxs -> HList xs

instance HConcatFD '[] '[] where
    hConcatFD _ = HNil

instance (HConcatFD as bs, HAppendFD a bs cs) => HConcatFD (HList a ': as) cs where
    hConcatFD (HCons x xs) = x `hAppendFD` hConcatFD xs

class HAppendFD a b ab | a b -> ab where
    hAppendFD :: HList a -> HList b -> HList ab

instance HAppendFD '[] b b where
    hAppendFD _ b = b

instance HAppendFD as bs cs => HAppendFD (a ': as) bs (a ': cs) where
    hAppendFD (HCons a as) bs = a `HCons` hAppendFD as bs


-- --------------------------------------------------------------------------
-- * traversing HLists

-- ** producing HList
-- *** map
-- $mapNote It could be implemented with 'hFoldr', as we show further below

{- | hMap is written such that the length of the result list
can be determined from the length of the argument list (and
the other way around). Similarly, the type of the elements
of the list is propagated in both directions too.

>>> :set -XNoMonomorphismRestriction
>>> let xs = 1 .*. 'c' .*. HNil
>>> :t hMap (HJust ()) xs
hMap (HJust ()) xs :: Num y => HList '[HJust y, HJust Char]


These 4 examples show that the constraint on the length (2 in this case)
can be applied before or after the 'hMap'. That inference is independent of the
direction that type information is propagated for the individual elements.


>>> let asLen2 xs = xs `asTypeOf` (undefined :: HList '[a,b])

>>> let lr xs = asLen2 (applyAB (HMap HRead) xs)
>>> let ls xs = asLen2 (applyAB (HMap HShow) xs)
>>> let rl xs = applyAB (HMap HRead) (asLen2 xs)
>>> let sl xs = applyAB (HMap HShow) (asLen2 xs)


>>> :t lr
lr
  :: (Read ..., Read ...) => HList '[String, String] -> HList '[..., ...]

>>> :t rl
rl
  :: (Read ..., Read ...) => HList '[String, String] -> HList '[..., ...]


>>> :t ls
ls
  :: (Show ..., Show ...) => HList '[..., ...] -> HList '[String, String]

>>> :t sl
sl
  :: (Show ..., Show ...) => HList '[..., ...] -> HList '[String, String]

-}

newtype HMap f = HMap f

hMap f xs = applyAB (HMap f) xs

instance (HMapCxt r f a b, as ~ r a, bs ~ r b)
    => ApplyAB (HMap f) as bs where
    applyAB (HMap f) = hMapAux f


-- | hMap constrained to HList
hMapL f xs = applyAB (HMapL f) xs

newtype HMapL f = HMapL f

instance (HMapCxt HList f a b, as ~ HList a, bs ~ HList b) => ApplyAB (HMapL f) as bs where
    applyAB (HMapL f) = hMapAux f


class (SameLength a b, HMapAux r f a b) => HMapCxt r f a b

instance (SameLength a b, HMapAux r f a b) => HMapCxt r f a b



class HMapAux (r :: [*] -> *) f (x :: [*]) (y :: [*]) where
  hMapAux :: SameLength x y => f -> r x -> r y

instance HMapAux HList f '[] '[] where
  hMapAux       _  _  = HNil

instance (ApplyAB f e e', HMapAux HList f l l', SameLength l l')
    => HMapAux HList f (e ': l) (e' ': l') where
  hMapAux f (HCons x l)    = applyAB f x `HCons` hMapAux f l




-- --------------------------------------------------------------------------

-- **** alternative implementation
-- $note currently broken

newtype MapCar f = MapCar f

-- | Same as 'hMap' only a different implementation.
hMapMapCar :: (HFoldr (MapCar f) (HList '[]) l l') =>
    f -> HList l -> l'
hMapMapCar f = hFoldr (MapCar f) HNil

instance ApplyAB f e e' => ApplyAB (MapCar f) (e,HList l) (HList (e' ': l)) where
    applyAB (MapCar f) (e,l) = HCons (applyAB f e) l


-- --------------------------------------------------------------------------

-- *** @appEndo . mconcat . map Endo@
{- |

>>> let xs = length .*. (+1) .*. (*2) .*. HNil
>>> hComposeList xs "abc"
8


-}
hComposeList
  :: (HFoldr Comp (a -> a) l (t -> a)) => HList l -> t -> a
hComposeList fs v0 = let r = hFoldr (Comp :: Comp) (\x -> x `asTypeOf` r) fs v0 in r


-- --------------------------------------------------------------------------

-- *** sequence
{- |
   A heterogeneous version of

   > sequenceA :: (Applicative m) => [m a] -> m [a]

   Only now we operate on heterogeneous lists, where different elements
   may have different types 'a'.
   In the argument list of monadic values (m a_i),
   although a_i may differ, the monad 'm' must be the same for all
   elements. That's why we needed "Data.HList.TypeCastGeneric2" (currently (~)).
   The typechecker will complain
   if we attempt to use hSequence on a HList of monadic values with different
   monads.

   The 'hSequence' problem was posed by Matthias Fischmann
   in his message on the Haskell-Cafe list on Oct 8, 2006

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018708.html>

   <http://www.haskell.org/pipermail/haskell-cafe/2006-October/018784.html>
 -}

class (Applicative m, SameLength a b) => HSequence m a b | a -> b, m b -> a where
    hSequence :: HList a -> m (HList b)
{- ^

[@Maybe@]

>>> hSequence $ Just (1 :: Integer) `HCons` (Just 'c') `HCons` HNil
Just H[1,'c']

>>> hSequence $  return 1 `HCons` Just  'c' `HCons` HNil
Just H[1,'c']


[@List@]

>>> hSequence $ [1] `HCons` ['c'] `HCons` HNil
[H[1,'c']]


-}

instance Applicative m => HSequence m '[] '[] where
    hSequence _ = pure HNil

instance (m1 ~ m, Applicative m, HSequence m as bs) =>
    HSequence m (m1 a ': as) (a ': bs) where
    hSequence (HCons a b) = liftA2 HCons a (hSequence b)

-- **** alternative implementation

-- | 'hSequence2' is not recommended over 'hSequence' since it possibly doesn't
-- allow inferring argument types from the result types. Otherwise this version
-- should do exactly the same thing.
--
-- The DataKinds version needs a little help to find the type of the
-- return HNil, unlike the original version, which worked just fine as
--
--  > hSequence l = hFoldr ConsM (return HNil) l

hSequence2 l =
    let rHNil = pure HNil `asTypeOf` (fmap undefined x)
        x = hFoldr (LiftA2 FHCons) rHNil l
    in x



-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- ** producing homogenous lists

-- *** map (no sequencing)
-- $mapOut This one we implement via hFoldr

newtype Mapcar f = Mapcar f

instance (l ~ [e'], ApplyAB f e e', el ~ (e,l)) => ApplyAB (Mapcar f) el l where
    applyAB (Mapcar f) (e, l) = applyAB f e : l

-- A synonym for the complex constraint
type HMapOut f l e = (HFoldr (Mapcar f) [e] l [e])

-- | compare @hMapOut f@ with @'hList2List' . 'hMap' f@
hMapOut :: forall f e l. HMapOut f l e => f -> HList l -> [e]
hMapOut f l = hFoldr (Mapcar f) ([] :: [e]) l


-- --------------------------------------------------------------------------
-- *** mapM

-- |
--
-- > mapM :: forall b m a. (Monad m) => (a -> m b) -> [a] -> m [b]
--
-- Likewise for 'mapM_'.
--
-- See 'hSequence' if the result list should also be heterogenous.

hMapM   :: (Monad m, HMapOut f l (m e)) => f -> HList l -> [m e]
hMapM f =  hMapOut f

-- | GHC doesn't like its own type.
--
-- > hMapM_ :: forall m a f e. (Monad m, HMapOut f a (m e)) => f -> a -> m ()
--
-- Without explicit type signature, it's Ok. Sigh.
-- Anyway, Hugs does insist on a better type. So we restrict as follows:
--
hMapM_   :: (Monad m, HMapOut f l (m ())) => f -> HList l -> m ()
hMapM_ f =  sequence_ .  disambiguate . hMapM f
 where
  disambiguate :: [q ()] -> [q ()]
  disambiguate =  id





-- --------------------------------------------------------------------------
-- * Ensure a list to contain HNats only
-- | We do so constructively, converting the HList whose elements
-- are Proxy HNat to [HNat]. The latter kind is unpopulated and
-- is present only at the type level.

type family HNats (l :: [*]) :: [HNat]
type instance HNats '[] = '[]
type instance HNats (Proxy n ': l) = n ': HNats l

hNats :: HList l -> Proxy (HNats l)
hNats _ = Proxy


-- --------------------------------------------------------------------------
-- * Membership tests

-- | Check to see if an HList contains an element with a given type
-- This is a type-level only test

class HMember (e1 :: k) (l :: [k]) (b :: Bool) | e1 l -> b
instance HMember e1 '[] False
instance (HEq e1 e b, HMember' b e1 l br) => HMember  e1 (e ': l) br
class HMember' (b0 :: Bool) (e1 :: k) (l :: [k]) (b :: Bool) | b0 e1 l -> b
instance HMember' True e1 l True
instance (HMember e1 l br) => HMember' False e1 l br

-- | The following is a similar type-only membership test
-- It uses the user-supplied curried type equality predicate pred
type family HMemberP pred e1 (l :: [*]) :: Bool
type instance HMemberP pred e1 '[] = False
type instance HMemberP pred e1 (e ': l) = HMemberP' pred e1 l (ApplyR pred (e1,e))

type family HMemberP' pred e1 (l :: [*]) pb :: Bool
type instance HMemberP' pred e1 l (Proxy True) = True
type instance HMemberP' pred e1 l (Proxy False) = HMemberP pred e1 l


hMember :: HMember e l b => Proxy e -> Proxy l -> Proxy b
hMember _ _ = Proxy

-- ** Another type-level membership test
--
-- | Check to see if an element e occurs in a list l
-- If not, return 'Nothing
-- If the element does occur, return 'Just l1
-- where l1 is a type-level list without e
class HMemberM (e1 :: k) (l :: [k]) (r :: Maybe [k]) | e1 l -> r
instance HMemberM e1 '[] 'Nothing
instance (HEq e1 e b, HMemberM1 b e1 (e ': l) res)
      =>  HMemberM e1 (e ': l) res

class HMemberM1 (b::Bool) (e1 :: k) (l :: [k]) (r::Maybe [k]) | b e1 l -> r
instance HMemberM1 True e1 (e ': l) ('Just l)
instance (HMemberM e1 l r, HMemberM2 r e1 (e ': l) res)
    => HMemberM1 False e1 (e ': l) res

class HMemberM2 (b::Maybe [k]) (e1 :: k) (l :: [k]) (r::Maybe [k]) | b e1 l -> r
instance HMemberM2 Nothing e1 l Nothing
instance HMemberM2 (Just l1) e1 (e ': l) (Just (e ': l1))

-- --------------------------------------------------------------------------

-- * Staged equality for lists
-- $note removed. use Typeable instead


{-
-- * Static set property based on HEq
class HSet l
instance HSet HNil
instance (HMember e l HFalse, HSet l) => HSet (HCons e l)
-}

-- * Find an element in a set based on HEq
-- | It is a pure type-level operation
class HFind1 e l l n => HFind (e :: k) (l :: [k]) (n :: HNat) | e l -> n
instance HFind1 e l l n => HFind e l n

class HFind1 (e :: k) (l :: [k]) (l0 :: [k]) (n :: HNat) | e l -> n

instance (HEq e1 e2 b, HFind2 b e1 l l0 n) => HFind1 e1 (e2 ': l) l0 n
instance Fail (FieldNotFound e1 l0) => HFind1 e1 '[] l0 HZero

class HFind2 (b::Bool) (e :: k) (l::[k]) (l0::[k]) (n:: HNat) | b e l -> n
instance HFind2 True e l l0 HZero
instance HFind1 e l l0 n => HFind2 False e l l0 (HSucc n)



-- ** Membership test based on type equality

-- | could be an associated type if HEq had one
class HTMember e (l :: [*]) (b :: Bool) | e l -> b
instance HTMember e '[] False
instance (HEq e e' b, HTMember e l b', HOr b b' ~ b'')
      =>  HTMember e (e' ': l) b''

hTMember :: HTMember e l b => e -> HList l -> Proxy b
hTMember _ _ = Proxy


-- * Intersection based on HTMember

class HTIntersect l1 l2 l3 | l1 l2 -> l3
 where
  -- | Like 'Data.List.intersect'
  hTIntersect :: HList l1 -> HList l2 -> HList l3

instance HTIntersect '[] l '[]
 where
  hTIntersect _ _ = HNil

instance ( HTMember h l1 b
         , HTIntersectBool b h t l1 l2
         )
         => HTIntersect (h ': t) l1 l2
 where
  hTIntersect (HCons h t) l1 = hTIntersectBool b h t l1
   where
    b = hTMember h l1

class HTIntersectBool (b :: Bool) h t l1 l2 | b h t l1 -> l2
 where
 hTIntersectBool :: Proxy b -> h -> HList t -> HList l1 -> HList l2

instance HTIntersect t l1 l2
      => HTIntersectBool True h t l1 (h ': l2)
 where
  hTIntersectBool _ h t l1 = HCons h (hTIntersect t l1)

instance HTIntersect t l1 l2
      => HTIntersectBool False h t l1 l2
 where
  hTIntersectBool _ _ t l1 = hTIntersect t l1


-- * Convert between heterogeneous lists and homogeneous ones

-- | @hMapOut id@ is similar, except this function is restricted
-- to HLists that actually contain a value (so the list produced
-- will be nonempty). This restriction allows adding a functional
-- dependency, which means that less type annotations can be necessary.
class HList2List l e | l -> e
 where
  hList2List :: HList l -> [e]
  list2HListSuffix :: [e] -> Maybe (HList l, [e])


list2HList :: HList2List l e => [e] -> Maybe (HList l)
list2HList = fmap fst . list2HListSuffix


instance HList2List '[e] e
 where
  hList2List (HCons e HNil) = [e]

  list2HListSuffix (e : es) = Just (HCons e HNil, es)
  list2HListSuffix [] = Nothing


instance HList2List (e' ': l) e
      => HList2List (e ': e' ': l) e
 where
  hList2List (HCons e l) = e:hList2List l

  list2HListSuffix (e : es) = (\(hl,rest) -> (HCons e hl, rest))
                                  <$> list2HListSuffix es
  list2HListSuffix [] = Nothing

-- | @Prism [s] [t] (HList s) (HList t)@
listAsHList x = prism hList2List (\l -> case list2HListSuffix l of
                                 Just (hl,[])  -> Right hl
                                 _ -> Left []) x

-- | @Prism' [a] (HList s)@
--
-- where @s ~ HReplicateR n a@
listAsHList' x = isSimple listAsHList x


-- --------------------------------------------------------------------------
-- * With 'HMaybe'

-- ** Turn list in a list of justs
-- | the same as @map Just@
--
-- >>> toHJust (2 .*. 'a' .*. HNil)
-- H[HJust 2,HJust 'a']
--
-- >>> toHJust2 (2 .*. 'a' .*. HNil)
-- H[HJust 2,HJust 'a']

class FromHJustR (ToHJustR l) ~ l => ToHJust l
 where
  type ToHJustR l :: [*]
  toHJust :: HList l -> HList (ToHJustR l)

instance ToHJust '[]
 where
  type ToHJustR '[] = '[]
  toHJust HNil = HNil

instance ToHJust l => ToHJust (e ': l)
 where
  type ToHJustR (e ': l) = HJust e ': ToHJustR l
  toHJust (HCons e l) = HCons (HJust e) (toHJust l)

-- | alternative implementation. The Apply instance is in "Data.HList.FakePrelude".
-- A longer type could be inferred.
toHJust2 :: (HMapCxt r (HJust ()) a b,
             ToHJust a, b ~ ToHJustR a -- added to get equivalent inference
             ) => r a -> r b
toHJust2 xs = hMap (HJust ()) xs

-- --------------------------------------------------------------------------
-- ** Extract justs from list of maybes
--
-- >>> let xs = 2 .*. 'a' .*. HNil
-- >>> fromHJust (toHJust xs) == xs
-- True

class (FromHJustR (ToHJustR l) ~ l) => FromHJust l
 where
  type FromHJustR l :: [*]
  fromHJust :: HList l -> HList (FromHJustR l)

instance FromHJust '[]
 where
  type FromHJustR '[] = '[]
  fromHJust HNil = HNil

instance FromHJust l => FromHJust (HNothing ': l)
 where
  type FromHJustR (HNothing ': l) = FromHJustR l
  fromHJust (HCons _ l) = fromHJust l

instance FromHJust l => FromHJust (HJust e ': l)
 where
  type FromHJustR (HJust e ': l) = e ': FromHJustR l
  fromHJust (HCons (HJust e) l) = HCons e (fromHJust l)

-- *** alternative implementation

-- | This implementation is shorter.
fromHJust2 :: (HMapCxt r HFromJust a b) => r a -> r b
fromHJust2 xs = hMap HFromJust xs

data HFromJust = HFromJust
instance (hJustA ~ HJust a) => ApplyAB HFromJust hJustA a where
    applyAB _ (HJust a) = a


-- --------------------------------------------------------------------------
-- * Annotated lists

data HAddTag t = HAddTag t
data HRmTag    = HRmTag

-- hAddTag :: HMapCxt (HAddTag t) l r => t -> HList l -> HList r
hAddTag t l = hMap (HAddTag t) l

-- hRmTag ::  HMapCxt HRmTag l => HList l -> HList (HMapR HRmTag l)
hRmTag l    = hMap HRmTag l

instance (et ~ (e,t)) => ApplyAB (HAddTag t) e et
 where
  applyAB (HAddTag t) e = (e,t)


instance (e' ~ e) => ApplyAB HRmTag (e,t) e'
 where
  applyAB _ (e,_) = e


-- | Annotate list with a type-level Boolean
--
-- > hFlag :: HMapCxt (HAddTag (Proxy True)) l r => HList l -> HList r
hFlag l = hAddTag hTrue l


-- --------------------------------------------------------------------------
-- * Splitting by HTrue and HFalse

-- | Analogus to Data.List.'Data.List.partition' 'snd'. See also 'HPartition'
--
-- >>> let (.=.) :: p x -> y -> Tagged x y; _ .=. y = Tagged y
-- >>> hSplit $ hTrue .=. 2 .*. hTrue .=. 3 .*. hFalse .=. 1 .*. HNil
-- (H[2,3],H[1])
--
-- it might make more sense to instead have @LVPair Bool e@
-- instead of @(e, Proxy Bool)@ since the former has the same
-- runtime representation as @e@

class HSplit l
 where
  type HSplitT l :: [*]
  type HSplitF l :: [*]
  hSplit :: HList l -> (HList (HSplitT l), HList (HSplitF l))

instance HSplit '[]
 where
  type HSplitT '[] = '[]
  type HSplitF '[] = '[]
  hSplit HNil = (HNil,HNil)

instance HSplit l => HSplit ((e, Proxy True) ': l)
 where

  type HSplitT ((e,Proxy True) ': l) = e ': HSplitT l
  type HSplitF ((e,Proxy True) ': l) = HSplitF l

  hSplit (HCons (e,_) l) = (HCons e l',l'')
   where
    (l',l'') = hSplit l

instance HSplit l => HSplit ((e,Proxy False) ': l)
 where
  type HSplitT ((e,Proxy False) ': l) = HSplitT l
  type HSplitF ((e,Proxy False) ': l) = e ': HSplitF l

  hSplit (HCons (e,_) l) = (l',HCons e l'')
   where
    (l',l'') = hSplit l


instance HSplit l => HSplit (Tagged True e ': l)
 where

  type HSplitT (Tagged True e ': l) = e ': HSplitT l
  type HSplitF (Tagged True e ': l) = HSplitF l

  hSplit (HCons (Tagged e) l) = (HCons e l',l'')
   where
    (l',l'') = hSplit l

instance HSplit l => HSplit (Tagged False e ': l)
 where
  type HSplitT (Tagged False e ': l) = HSplitT l
  type HSplitF (Tagged False e ': l) = e ': HSplitF l

  hSplit (HCons (Tagged e) l) = (l',HCons e l'')
   where
    (l',l'') = hSplit l
{-

Let expansion makes a difference to Hugs:

HListPrelude> let x = (hFlag (HCons "1" HNil)) in hSplit x
(HCons "1" HNil,HNil)
HListPrelude> hSplit (hFlag (HCons "1" HNil))
ERROR - Unresolved overloading
*** Type       : HSplit (HCons ([Char],HTrue) HNil) a b => (a,b)
*** Expression : hSplit (hFlag (HCons "1" HNil))


-}

-- --------------------------------------------------------------------------
-- * Splitting by Length

{- | 'splitAt'

setup

>>> let two = hSucc (hSucc hZero)
>>> let xsys = hEnd $ hBuild 1 2 3 4

If a length is explicitly provided, the resulting lists are inferred

>>> hSplitAt two xsys
(H[1,2],H[3,4])

>>> let sameLength_ :: SameLength a b => r a -> r b -> r a; sameLength_ = const
>>> let len2 x = x `sameLength_` HCons () (HCons () HNil)

If the first chunk of the list (a) has to be a certain length, the type of the
Proxy argument can be inferred.

>>> case hSplitAt Proxy xsys of (a,b) -> (len2 a, b)
(H[1,2],H[3,4])

-}
class (HLengthEq xs n,
       HAppendList1 xs ys xsys
       )
      => HSplitAt (n :: HNat) xsys xs ys
                   | n xsys -> xs ys
                      , xs ys -> xsys
                      , xs -> n
                     where
    hSplitAt :: Proxy n -> HList xsys -> (HList xs, HList ys)

instance (HSplitAt1 '[] n xsys xs ys,
          HAppendList1 xs ys xsys,
          HLengthEq xs n) =>
    HSplitAt n xsys xs ys where
      hSplitAt n xsys = hSplitAt1 HNil n xsys

-- | helper for 'HSplitAt'
class HSplitAt1 accum (n :: HNat) xsys xs ys | accum n xsys -> xs ys where
    hSplitAt1 :: HList accum -> Proxy n -> HList xsys -> (HList xs, HList ys)

instance HRevApp accum '[] xs => HSplitAt1 accum HZero ys xs ys where
    hSplitAt1 xs _zero ys = (hReverse_ xs, ys)

instance HSplitAt1 (b ': accum) n bs xs ys
    => HSplitAt1 accum (HSucc n) (b ': bs) xs ys where
    hSplitAt1 accum n (HCons b bs) = hSplitAt1 (HCons b accum) (hPred n) bs

{- | a better way to write @HLength xs ~ n@ because:

1. it works properly with ghc-7.10 (probably another example of ghc bug #10009)

2. it works backwards a bit in that if @n@ is known, then @xs@ can be
   refined:

>>> undefined :: HLengthEq xs HZero => HList xs
H[]

-}
class (SameLength' (HReplicateR n ()) xs,
        HLengthEq1 xs n, HLengthEq2 xs n) => HLengthEq (xs :: [*]) (n :: HNat) | xs -> n

instance (SameLength' (HReplicateR n ()) xs,
          HLengthEq1 xs n, HLengthEq2 xs n) => HLengthEq xs n

class HLengthEq1 (xs :: [*]) n -- pick the instance based on n's constructor
instance (HLengthEq xs n, xxs ~ (x ': xs)) => HLengthEq1 xxs (HSucc n)
instance (xxs ~ '[]) => HLengthEq1 xxs HZero

class HLengthEq2 (xs :: [*]) n | xs -> n -- pick the instance based on xs' constructor
instance (HLengthEq xs n, sn ~ HSucc n) => HLengthEq2 (x ': xs) sn
instance zero ~ HZero => HLengthEq2 '[] zero

-- | @HLengthGe xs n@ says that @HLength xs >= n@.
--
-- unlike the expression with a type family HLength,
-- ghc assumes @xs ~ (aFresh ': bFresh)@ when given a
-- constraint @HLengthGe xs (HSucc HZero)@
class HLengthGe (xs :: [*]) (n :: HNat)
instance (HLengthGe xs n, xxs ~ (x ': xs)) => HLengthGe xxs (HSucc n)
instance HLengthGe xxs HZero

-- | @HAppendList1 xs ys xsys@ is the type-level way of saying @xs ++ ys == xsys@
--
-- used by 'HSplitAt'
class HStripPrefix xs xsys ys
      => HAppendList1 (xs :: [k]) (ys :: [k]) (xsys :: [k])
        | xs ys -> xsys,
          xs xsys -> ys
          -- , ys xsys -> xs
          -- hard to prove

instance HAppendList1 '[] ys ys
instance (HAppendList1 xs ys zs) => HAppendList1 (x ': xs) ys (x ': zs)


-- | analog of 'Data.List.stripPrefix'
class HStripPrefix xs xsys ys | xs xsys -> ys
instance (x' ~ x, HStripPrefix xs xsys ys) => HStripPrefix (x' ': xs) (x ': xsys) ys
instance HStripPrefix '[] ys ys


-- ** take

class HTake (n :: HNat) xs ys | n xs -> ys where
    hTake :: (HLengthEq ys n, HLengthGe xs n) => Proxy n -> HList xs -> HList ys

instance HTake HZero xs '[] where
    hTake _ _ = HNil

instance (HLengthEq ys n, HLengthGe xs n, HTake n xs ys)
        => HTake (HSucc n) (x ': xs) (x ': ys) where
    hTake sn (HCons x xs) = HCons x (hTake (hPred sn) xs)

-- ** drop

class HDrop (n :: HNat) xs ys | n xs -> ys where
    hDrop :: HLengthGe xs n => Proxy n -> HList xs -> HList ys

instance HDrop HZero xs xs where
    hDrop _ xs = xs

instance (HLengthGe xs n, HDrop n xs ys) => HDrop (HSucc n) (x ': xs) ys where
    hDrop sn (HCons _ xs) = hDrop (hPred sn) xs


-- * Conversion to and from tuples

class HTuple v t | v -> t, t -> v where
    -- | alternatively: @hUncurry (,,,)@
    hToTuple :: HList v -> t
    hFromTuple :: t -> HList v

-- | @Iso (HList v) (HList v') a b@
hTuple x = iso hToTuple hFromTuple x

-- | @Iso' (HList v) a@
hTuple' x = isSimple hTuple x

instance HTuple '[] () where
    hToTuple HNil = ()
    hFromTuple () = HNil

instance HTuple '[a,b] (a,b) where
    hToTuple (a `HCons` b `HCons` HNil) = (a,b)
    hFromTuple (a,b) = (a `HCons` b `HCons` HNil)

instance HTuple '[a,b,c] (a,b,c) where
    hToTuple (a `HCons` b `HCons` c `HCons` HNil) = (a,b,c)
    hFromTuple (a,b,c) = (a `HCons` b `HCons` c `HCons` HNil)

instance HTuple '[a,b,c,d] (a,b,c,d) where
    hToTuple (a `HCons` b `HCons` c `HCons` d `HCons` HNil) = (a,b,c,d)
    hFromTuple (a,b,c,d) = (a `HCons` b `HCons` c `HCons` d `HCons` HNil)

instance HTuple '[a,b,c,d,e] (a,b,c,d,e) where
    hToTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil) = (a,b,c,d,e)
    hFromTuple (a,b,c,d,e) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil)

instance HTuple '[a,b,c,d,e,f] (a,b,c,d,e,f) where
    hToTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil) = (a,b,c,d,e,f)
    hFromTuple (a,b,c,d,e,f) = (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil)


-- | 'tails'
class HTails a b | a -> b, b -> a where
    hTails :: HList a -> HList b

instance HTails '[] '[HList '[]] where
    hTails _ = HCons HNil HNil

instance (HTails xs ys) => HTails (x ': xs) (HList (x ': xs) ': ys) where
    hTails xxs@(HCons _x xs) = xxs `HCons` hTails xs


-- | 'inits'
class HInits a b | a -> b, b -> a where
    hInits :: HList a -> HList b

instance HInits1 a b => HInits a (HList '[] ': b) where
    hInits xs = HNil `HCons` hInits1 xs


-- | behaves like @tail . inits@
class HInits1 a b | a -> b, b -> a where
    hInits1 :: HList a -> HList b

instance HInits1 '[] '[HList '[]] where
    hInits1 _ = HCons HNil HNil

instance (HInits1 xs ys,
          HMapCxt HList (FHCons2 x) ys ys',
          HMapCons x ys ~ ys',
          HMapTail ys' ~ ys)
  => HInits1 (x ': xs) (HList '[x] ':  ys') where
    hInits1 (HCons x xs) = HCons x HNil `HCons` hMap (FHCons2 x) (hInits1 xs)


-- | similar to 'FHCons'
data FHCons2 x = FHCons2 x

instance (hxs ~ HList xs,
          hxxs ~ HList (x ': xs))
  => ApplyAB (FHCons2 x) hxs hxxs where
  applyAB (FHCons2 x) xs = HCons x xs


-- | evidence to satisfy the fundeps in HInits
type family HMapCons (x :: *) (xxs :: [*]) :: [*]
type instance HMapCons x (HList a ': b) = HList (x ': a) ': HMapCons x b
type instance HMapCons x '[] = '[]

-- | evidence to satisfy the fundeps in HInits
type family HMapTail (xxs :: [*]) :: [*]
type instance HMapTail ( HList (a ': as) ': bs) = HList as ': HMapTail bs
type instance HMapTail '[] = '[]


-- * partition

{- | @HPartitionEq f x1 xs xi xo@ is analogous to

> (xi,xo) = partition (f x1) xs

where @f@ is a \"function\" passed in using it's instance of 'HEqBy'
-}
class HPartitionEq f x1 xs xi xo | f x1 xs -> xi xo where
    hPartitionEq :: Proxy f -> Proxy x1 -> HList xs -> (HList xi, HList xo)

instance HPartitionEq f x1 '[] '[] '[] where
    hPartitionEq _ _ _ = (HNil, HNil)

instance
   (HEqBy f x1 x b,
    HPartitionEq1 b f x1 x xs xi xo) => HPartitionEq f x1 (x ': xs) xi xo where
      hPartitionEq f x1 (HCons x xs) = hPartitionEq1 (Proxy :: Proxy b) f x1 x xs

class HPartitionEq1 (b :: Bool) f x1 x xs xi xo | b f x1 x xs -> xi xo where
    hPartitionEq1 :: Proxy b -> Proxy f -> Proxy x1 -> x -> HList xs -> (HList xi, HList xo)

instance HPartitionEq f x1 xs xi xo =>
    HPartitionEq1 True f x1 x xs (x ': xi) xo where
      hPartitionEq1 _ f x1 x xs = case hPartitionEq f x1 xs of
         (xi, xo) -> (x `HCons` xi, xo)

instance HPartitionEq f x1 xs xi xo =>
    HPartitionEq1 False f x1 x xs xi (x ': xo) where
      hPartitionEq1 _ f x1 x xs = case hPartitionEq f x1 xs of
         (xi, xo) -> (xi, x `HCons` xo)


-- * groupBy

{- | @HGroupBy f x y@ is analogous to @y = 'groupBy' f x@

given that @f@ is used by 'HEqBy'
-}
class HGroupBy (f :: t) (as :: [*]) (gs :: [*]) | f as -> gs, gs -> as where
    hGroupBy :: Proxy f -> HList as -> HList gs

instance (HSpanEqBy f a as fst snd,
          HGroupBy f snd gs) => HGroupBy f (a ': as) (HList (a ': fst) ': gs) where
    hGroupBy f (HCons x xs) = case hSpanEqBy f x xs of
                      (first, second) -> (x `HCons` first) `HCons` hGroupBy f second

instance HGroupBy f '[] '[] where
    hGroupBy _f HNil = HNil

-- * span

-- | @HSpanEq x y fst snd@ is analogous to @(fst,snd) = 'span' (== x) y@
class HSpanEqBy (f :: t) (x :: *) (y :: [*]) (fst :: [*]) (snd :: [*])
      | f x y -> fst snd, fst snd -> y where
  hSpanEqBy :: Proxy f -> x -> HList y -> (HList fst, HList snd)

instance (HSpanEqBy1 f x y fst snd,
          HAppendListR fst snd ~ y)
    => HSpanEqBy f x y fst snd where
  hSpanEqBy f x y =  hSpanEqBy1 f x y

class HSpanEqBy1 (f :: t) (x :: *) (y :: [*]) (i :: [*]) (o :: [*])
      | f x y -> i o where
  hSpanEqBy1 :: Proxy f -> x -> HList y -> (HList i, HList o)

class HSpanEqBy2 (b :: Bool) (f :: t) (x :: *) (y :: *) (ys :: [*]) (i :: [*]) (o :: [*])
      | b f x y ys -> i o where
  hSpanEqBy2 :: Proxy b -> Proxy f -> x -> y -> HList ys -> (HList i, HList o)


instance (HEqBy f x y b,
          HSpanEqBy2 b f x y ys i o) => HSpanEqBy1 f x (y ': ys) i o where
  hSpanEqBy1 f x (HCons y ys) = hSpanEqBy2 (Proxy :: Proxy b) f x (y :: y) (ys :: HList ys)

instance HSpanEqBy1 f x '[] '[] '[] where
    hSpanEqBy1 _f _x _xs = (HNil, HNil)

instance HSpanEqBy1 f x zs i o
    => HSpanEqBy2 True f x y zs (y ': i) o where
  hSpanEqBy2 _ f x y zs = case hSpanEqBy1 f x zs of
                                      (i, o) -> (HCons y i, o)

instance HSpanEqBy2 False f x y ys '[] (y ': ys) where
  hSpanEqBy2 _b _f _x y ys = (HNil, HCons y ys)



-- * zip

-- $note see alternative implementations in "Data.HList.HZip"



instance (SameLengths [x,y,xy], HZipList x y xy) => HUnzip HList x y xy where
  hUnzip = hUnzipList

instance (SameLengths [x,y,xy], HZipList x y xy) => HZip HList x y xy where
  hZip = hZipList


class HZipList x y l | x y -> l, l -> x y where
  hZipList   :: HList x -> HList y -> HList l
  hUnzipList :: HList l -> (HList x, HList y)

instance HZipList '[] '[] '[] where
  hZipList _ _ = HNil
  hUnzipList _ = (HNil, HNil)

instance ((x,y)~z, HZipList xs ys zs) => HZipList (x ': xs) (y ': ys) (z ': zs) where
  hZipList (HCons x xs) (HCons y ys) = (x,y) `HCons` hZipList xs ys
  hUnzipList (HCons ~(x,y) zs) = let ~(xs,ys) = hUnzipList zs in (x `HCons` xs, y `HCons` ys)

-- * Monoid instance

{- | Analogous to the Monoid instance for tuples

>>> import Data.Monoid
>>> mempty :: HList '[(), All, [Int]]
H[(),All {getAll = True},[]]

>>> mappend (hBuild "a") (hBuild "b") :: HList '[String]
H["ab"]

-}
instance
   (HProxies a,
    HMapCxt HList ConstMempty (AddProxy a) a,
    HZip HList a a aa,
    HMapCxt HList UncurryMappend aa a) => Monoid (HList a) where
  mempty = hMap ConstMempty
            $ (hProxies :: HList (AddProxy a))
#if __GLASGOW_HASKELL__ <= 906
  mappend a b = hMap UncurryMappend $ hZip a b
#endif

instance
    (HZip HList a a aa,
     HMapCxt HList UncurryMappend aa a) => Semigroup (HList a) where
  a <> b = hMap UncurryMappend $ hZip a b

-- ** helper functions

data ConstMempty = ConstMempty
instance (x ~ Proxy y, Monoid y) => ApplyAB ConstMempty x y where
    applyAB _ _ = mempty

data UncurryMappend = UncurryMappend
instance (aa ~ (a,a), Monoid a) => ApplyAB UncurryMappend aa a where
    applyAB _ = uncurry mappend

data UncurrySappend = UncurrySappend
instance (aa ~ (a,a), Semigroup a) => ApplyAB UncurrySappend aa a where
    applyAB _ = uncurry (<>)
