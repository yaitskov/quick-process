{-# LANGUAGE CPP #-}

{- |
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.
   The public interface is described in
   <Data-HList-CommonMain.html#t:TIP CommonMain#TIP>
-}

module Data.HList.TIP
  (module Data.HList.TIPtuple,
   module Data.HList.TIP) where


import Data.HList.HListPrelude
import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.HTypeIndexed ()
import Data.HList.TIPtuple
import Data.List (intercalate)
import Data.Array (Ix)
#if __GLASGOW_HASKELL__ <= 906
import Data.Semigroup (Semigroup)
#endif

#if __GLASGOW_HASKELL__ > 710
import Data.Coerce
#endif

import LensDefs

-- --------------------------------------------------------------------------
-- * The newtype for type-indexed products

-- | TIPs are like 'Record', except element \"i\" of the list \"l\"
-- has type @Tagged e_i e_i@
newtype TIP (l :: [*]) = TIP{unTIP:: HList l}

deriving instance Semigroup (HList a) => Semigroup (TIP a)
deriving instance Monoid (HList a) => Monoid (TIP a)
deriving instance Eq (HList a) => Eq (TIP a)
deriving instance (Ord (HList r)) => Ord (TIP r)
deriving instance (Ix (HList r)) => Ix (TIP r)
deriving instance (Bounded (HList r)) => Bounded (TIP r)


instance HMapOut (HShow `HComp` HUntag) l String => Show (TIP l) where
  showsPrec _ (TIP l) = ("TIPH[" ++)
                              . (intercalate "," (hMapOut (HShow `HComp` HUntag) l) ++)
                              . (']' :)


mkTIP :: HTypeIndexed l => HList l -> TIP l
mkTIP = TIP

emptyTIP :: TIP '[]
emptyTIP = mkTIP HNil

-- --------------------------------------------------------------------------
-- * Type-indexed type sequences

-- | this constraint ensures that a TIP created by 'mkTIP' has no
-- duplicates
class (HAllTaggedEq l, HRLabelSet l) => HTypeIndexed (l :: [*])

instance (HAllTaggedEq l, HRLabelSet l) => HTypeIndexed l

class HAllTaggedEq (l :: [*])
instance HAllTaggedEq '[]
instance (HAllTaggedEq l, tee ~ Tagged e e') => HAllTaggedEq (tee ': l)

-- --------------------------------------------------------------------------
-- Implementing the HListPrelude interface

instance (HRLabelSet (Tagged e e ': l), HTypeIndexed l) => HExtend e (TIP l)
 where
  type HExtendR e (TIP l) = TIP (Tagged e e ': l)
  e .*. TIP l = mkTIP (HCons (Tagged e) l)



instance (e ~ e', HasField e (Record l) e') => HasField e (TIP l) e' where
    hLookupByLabel lab (TIP l) = hLookupByLabel lab (Record l)

-- | One occurrence and nothing is left
--
-- This variation provides an extra feature for singleton lists.
-- That is, the result type is unified with the element in the list.
-- Hence the explicit provision of a result type can be omitted.
--

instance (tee ~ Tagged e e) => HOccurs e (TIP '[tee]) where
  hOccurs (TIP (HCons (Tagged e) _)) = e

instance HasField e (Record (x ': y ': l)) e
      => HOccurs e (TIP (x ': y ': l)) where
  hOccurs (TIP l) = Record l .!. (Label :: Label e)


instance (HAppend (HList l) (HList l'), HTypeIndexed (HAppendListR l l'))
           => HAppend (TIP l) (TIP l')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')

type instance HAppendR (TIP l) (TIP l') = TIP (HAppendListR l l')


-- instance HOccurrence HList e l l' => HOccurrence TIP e l l'
--  where
--   hOccurrence e = TIP . hOccurrence e . unTIP

-- --------------------------------------------------------------------------
-- * Shielding type-indexed operations
-- $note The absence of signatures is deliberate! They all must be inferred.

onRecord f (TIP l) = let Record l' = f (Record l) in mkTIP l'

instance (HDeleteAtLabel Record e v v',
          HTypeIndexed v')
      => HDeleteAtLabel TIP e v v' where
  hDeleteAtLabel e v = onRecord (hDeleteAtLabel e) v


tipyUpdate  e t  = hTPupdateAtLabel (fromValue e) e t
  where fromValue :: e -> Label e
        fromValue _ = Label

instance (HUpdateAtLabel Record e' e r r',
          HTypeIndexed r',
         e ~ e') => HUpdateAtLabel TIP e' e r r' where
  hUpdateAtLabel l e r = onRecord (hUpdateAtLabel l e) r


-- | Use 'Labels' to specify the first argument
tipyProject ps t = onRecord (hProjectByLabels ps) t

-- | provides a @Lens' (TIP s) a@. 'hLens'' @:: Label a -> Lens' (TIP s) a@
-- is another option.
#if __GLASGOW_HASKELL__ < 707
tipyLens' x = isSimple tipyLens x -- rejected by GHC-7.10RC1
#else
tipyLens' f s = isSimple (hLens x) f (asTIP s) -- rejected by GHC-7.6.3
  where
    x = getA f
    getA :: (a -> f a) -> Label a
    getA _ = Label

    asTIP :: TIP a -> TIP a
    asTIP = id
#endif

{- | provides a @Lens (TIP s) (TIP t) a b@

When using @set@ (also known as @.~@), 'tipyLens'' can address the
ambiguity as to which field \"a\" should actually be updated.

-}
tipyLens f (TIP s) =
      case hSplitAt (getN s f) (ghc8fix1 s) of
          (x, ta@(Tagged a) `HCons` ys)
             | () <- ghc8fix2 ta ->
              let mkt b = mkTIP (x `hAppendList` (tagSelf b `HCons` ys))
              in mkt <$> f a
  where
    getN :: HFind (Label a) (LabelsOf s) n => HList s -> (a -> f b) -> Proxy n
    getN _ _ = Proxy

    -- without these, tipyLens has a type that has kind variables,
    -- (that end up being * when an actual TIP is provided), leading to
    -- a Properties.LengthIndependent compile error:
    -- .../.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/Data/HList/TIP.hi
    -- Declaration for tipyLens:
    --   Iface type variable out of scope:  k
    -- Cannot continue after interface file error
    ghc8fix1 :: HList (Tagged x x ': xs) -> HList (Tagged x x ': xs)
    ghc8fix1 = id

    ghc8fix2 :: Tagged a a -> ()
    ghc8fix2 _ = ()



-- | The same as 'tipyProject', except also return the
-- types not requested in the @proxy@ argument
tipyProject2 ps (TIP l) = (mkTIP l',mkTIP l'')
 where
  (l',l'') = h2projectByLabels ps l


-- --------------------------------------------------------------------------

-- | Subtyping for TIPs

instance SubType (TIP l) (TIP '[])
instance (HOccurs e (TIP l1), SubType (TIP l1) (TIP l2))
      =>  SubType (TIP l1) (TIP (e ': l2))


-- --------------------------------------------------------------------------
-- * conversion to and from 'HList'

{- | 'TagR' can also be used to avoid redundancy when defining types for TIC and TIP.

>  type XShort = TagR [A,B,C,D]

>  type XLong = [Tagged A A, Tagged B B, Tagged C C, Tagged D D]


an equivalent FD version, which is slightly better with respect to
simplifying types containing type variables (in ghc-7.8 and 7.6):
<http://stackoverflow.com/questions/24110410/>

With ghc-7.10 (<http://ghc.haskell.org/trac/ghc/ticket/10009>) the FD version is superior
to the TF version:

@
class (UntagR (TagR a) ~ a) => TagUntag a where
    type TagR a :: [*]
    hTagSelf :: HList a -> HList (TagR a)
    hUntagSelf :: HList (TagR a) -> HList a

instance TagUntag '[] where
    type TagR '[] = '[]
    hTagSelf _ = HNil
    hUntagSelf _ = HNil

instance TagUntag xs => TagUntag (x ': xs) where
    type TagR (x ': xs) = Tagged x x ': TagR xs
    hTagSelf (HCons x xs) = Tagged x `HCons` hTagSelf xs
    hUntagSelf (HCons (Tagged x) xs) = x `HCons` hUntagSelf xs

type family UntagR (xs :: [*]) :: [*]
type instance UntagR '[] = '[]
type instance UntagR (x ': xs) = Untag1 x ': UntagR xs
@

Length information should flow backwards

>>> let len2 x = x `asTypeOf` (undefined :: HList '[a,b])
>>> let f = len2 $ hTagSelf (hReplicate Proxy ())
>>> :t f
f :: HList '[Tagged () (), Tagged () ()]

-}
class SameLength a ta => TagUntagFD a ta | a -> ta, ta -> a where
    hTagSelf :: HList a -> HList ta
    hUntagSelf :: HList ta -> HList a

instance TagUntagFD '[] '[] where
    hTagSelf _ = HNil
    hUntagSelf _ = HNil

instance (TagUntagFD xs ys, txx ~ Tagged x x)
      => TagUntagFD (x ': xs) (txx ': ys) where
    hTagSelf (HCons x xs) = Tagged x `HCons` hTagSelf xs
    hUntagSelf (HCons (Tagged x) xs) = x `HCons` hUntagSelf xs

type TagUntag xs = TagUntagFD xs (TagR xs)

-- | Sometimes the type variables available have @TagR@ already applied
-- (ie the lists have elements like @Tagged X X@). Then this abbreviation
-- is useful:
type UntagTag xs = (TagR (UntagR xs) ~ xs, TagUntagFD (UntagR xs) xs)

type family TagR (a :: [*]) :: [*]
type family UntagR (ta :: [*]) :: [*]

type instance TagR '[] = '[]
type instance UntagR '[] = '[]

type instance TagR (x ': xs) = Tagged x x ': TagR xs
type instance UntagR (Tagged y y ': ys) = y ': UntagR ys

type family Untag1 (x :: *) :: *
type instance Untag1 (Tagged k x) = x


-- | @Iso (TIP (TagR a)) (TIP (TagR b)) (HList a) (HList b)@
tipHList x = iso (\(TIP a) -> hUntagSelf a) (TIP . hTagSelf) x

-- | @Iso' (TIP (TagR s)) (HList a)@
tipHList' x = isSimple tipHList x


-- * conversion to and from 'Record'

-- | @Iso (TIP s) (TIP t) (Record s) (Record t)@
--
-- 'typeIndexed' may be more appropriate
tipRecord x = isoNewtype (\(TIP a) -> Record a) (\(Record b) -> TIP b) x

-- | @Iso' (TIP (TagR s)) (Record a)@
tipRecord' x = isSimple tipRecord x

-- --------------------------------------------------------------------------
-- * Zip

#if __GLASGOW_HASKELL__ < 800
-- pre-coerce
instance (HZipList (UntagR x) (UntagR y) (UntagR xy),
          UntagTag x, UntagTag y, UntagTag xy,
          SameLengths [x,y,xy],
          HTypeIndexed x, HTypeIndexed y,
          HUnzip TIP x y xy
          -- HTypeIndexed xy is always satisfied given the above
          -- constraints (with a handwaving proof), so don't require
          -- callers of hZip/hUnzip to supply such proof
    ) => HZip TIP x y xy where
  hZip = hZipTIP


instance (HZipList (UntagR x) (UntagR y) (UntagR xy),
          UntagTag x, UntagTag y, UntagTag xy,
          HTypeIndexed x, HTypeIndexed y,
          SameLengths [x,y,xy]) => HUnzip TIP x y xy where
  hUnzip = hUnzipTIP

#else
-- ghc-7.10.3 has coerce, but rejects these instances
instance (HZipList xL yL xyL,
          lty ~ (HList xyL -> (HList xL,HList yL)),
          Coercible lty (TIP xy -> (TIP x, TIP y)),
          UntagR x ~ xL, TagR xL ~ x, -- `TagR (UntagR x) ~ x` included by UntagTag
          UntagR y ~ yL, TagR yL ~ y,
          UntagR xy ~ xyL, TagR xyL ~ xy,
          SameLengths '[x,y,xy],
          UntagTag x, UntagTag y, UntagTag xy
        ) => HUnzip TIP x y xy where
  hUnzip = coerce (hUnzipList :: lty)

instance (HUnzip TIP x y xy,
          HZipList xL yL xyL,
          lty ~ (HList xL -> HList yL -> HList xyL),
          Coercible lty (TIP x -> TIP y -> TIP xy) ,
          UntagR x ~ xL,
          UntagR y ~ yL,
          UntagR xy ~ xyL,
          UntagTag x, UntagTag y, UntagTag xy
        ) => HZip TIP x y xy where
  hZip = coerce (hZipList :: lty)
#endif

-- | specialization of 'hZip'
hZipTIP (TIP x) (TIP y) = TIP (hTagSelf (hZipList (hUntagSelf x) (hUntagSelf y)))

-- | specialization of 'hUnzip'
hUnzipTIP (TIP xy) = case hUnzipList (hUntagSelf xy) of
  (x,y) -> (mkTIP (hTagSelf x), mkTIP (hTagSelf y))



-- --------------------------------------------------------------------------
-- * TIP Transform

{- |

Transforming a TIP: applying to a TIP a (polyvariadic) function
that takes arguments from a TIP and updates the TIP with the result.

In more detail: we have a typed-indexed collection TIP and we
would like to apply a transformation function to it, whose argument
types and the result type are all in the TIP. The function should locate
its arguments based on their types, and update the TIP
with the result. The function may have any number of arguments,
including zero; the order of arguments should not matter.

The problem was posed by Andrew U. Frank on Haskell-Cafe, Sep 10, 2009.
<http://www.haskell.org/pipermail/haskell-cafe/2009-September/066217.html>
The problem is an interesting variation of the keyword argument problem.

Examples can be found in @examples/TIPTransform.hs@ and @examples/TIPTransformM.hs@
-}

class TransTIP op db where
    ttip :: op -> TIP db -> TIP db

instance (HMember (Tagged op op) db b,
          Arity op n,
          TransTIP1 b n op db)
    => TransTIP op db where
    ttip = ttip1 (Proxy ::Proxy b) (Proxy :: Proxy n)

class TransTIP1 (b :: Bool) (n :: HNat) op db where
    ttip1 :: Proxy b -> Proxy n -> op -> TIP db -> TIP db

-- If op is found in a TIP, update the TIP with op
instance HTPupdateAtLabel TIP op op db
    => TransTIP1 True n op db where
    ttip1 _ _ = tipyUpdate

-- If op is not found in a TIP, it must be a function. Try to look up
-- its argument in a TIP and recur.
instance (HMember (Tagged arg arg) db b,
          TransTIP2 b arg op db)
    => TransTIP1 False (HSucc n) (arg -> op) db where
    ttip1 _ _ = ttip2 (Proxy :: Proxy b)

instance Fail (FieldNotFound notfun (TIP db))
      => TransTIP1 False HZero notfun db where
    ttip1 = error "TransTIP1 Fail failed"

class TransTIP2 (b :: Bool) arg op db where
    ttip2 :: Proxy b -> (arg -> op) -> TIP db -> TIP db

instance (HOccurs arg (TIP db),
         TransTIP op db)
   => TransTIP2 True arg op db where
    ttip2 _ f db = ttip (f (hOccurs db)) db

instance Fail (FieldNotFound arg (TIP db))
    => TransTIP2 False arg op db where
    ttip2 = error "TransTIP2 Fail failed"

-- ** Monadic version

{- |

In March 2010, Andrew Frank extended the problem for monadic operations.
This is the monadic version of @TIPTransform.hs@ in the present directory.

This is the TF implementation. When specifying the operation to perform over
a TIP, we can leave it polymorphic over the monad. The type checker
will instantiate the monad based on the context.

-}
class Monad m => TransTIPM m op db where
    ttipM :: op -> TIP db -> m (TIP db)

-- Check to see if the operation is a computation whose result
-- is in the TIP. The type variable m' of the kind *->* below
-- can be instantiated either to a monad type constructor, or (arg->).
instance (Monad m, HMember (Tagged op op) db b,
           Arity (m' op) n,
           TransTIPM1 b n m (m' op) db)
    => TransTIPM m (m' op) db where
    ttipM = ttipM1 (Proxy :: Proxy b) (Proxy :: Proxy n)

class Monad m => TransTIPM1 (b :: Bool) (n :: HNat) m op db where
    ttipM1 :: Proxy b -> Proxy n -> op -> TIP db -> m (TIP db)

-- If op is found in a TIP, update the TIP with op.
-- The type variable m' must be equal to the type of the monad
-- in which the final result is reported.
instance (Monad m, m ~ m', HTPupdateAtLabel TIP op op db)
    => TransTIPM1 True n m (m' op) db where
    ttipM1 _ _ op db = do
         op' <- op
         return $ tipyUpdate op' db

instance (Fail (FieldNotFound op (TIP db)), Monad m)
    => TransTIPM1 False HZero m op db where
    ttipM1 _ _ = error "TransTIPM1 Fail failed"

-- If op is not found in a TIP, it must be a function. Look up
-- its argument in a TIP and recur.
instance (Monad m,
          HMember (Tagged arg arg) db b,
          TransTIPM2 b m arg op db)
    => TransTIPM1 False (HSucc n) m (arg-> op) db where
    ttipM1 _ _ = ttipM2 (Proxy :: Proxy b)


class TransTIPM2 (b :: Bool) m arg op db where
    ttipM2 :: Proxy b -> (arg -> op) -> TIP db -> m (TIP db)

instance (HOccurs arg (TIP db), TransTIPM m op db)
      => TransTIPM2 True m arg op db where
    ttipM2 _ f db = ttipM (f (hOccurs db)) db


instance Fail (FieldNotFound op (TIP db))
    => TransTIPM2 False m arg op db where
    ttipM2 _ _ = error "TransTIPM1 Fail failed"

-- --------------------------------------------------------------------------

-- tests for tipyTuple. These only work if tipyTuple is compiled
-- in a module that has NoMonoLocalBinds enabled
_ = tipyTuple ( '1' .*. True .*. emptyTIP ) :: (Char, Bool)
_ = tipyTuple ( '1' .*. True .*. emptyTIP ) :: (Bool, Char)


-- --------------------------------------------------------------------------

-- * Sample code

{- $setup

[@Assume@]

>>> import Data.HList.TypeEqO
>>> import Data.HList.FakePrelude
>>> import Data.HList.HOccurs

>>> :{
newtype Key    = Key Integer deriving (Show,Eq,Ord)
newtype Name   = Name String deriving (Show,Eq)
data Breed     = Cow | Sheep deriving (Show,Eq)
newtype Price  = Price Float deriving (Show,Eq,Ord)
data Disease   = BSE | FM deriving (Show,Eq)
type Animal =  TagR '[Key,Name,Breed,Price]
:}

>>> :{
let myTipyCow :: TIP Animal -- optional
    myTipyCow = Key 42 .*.  Name "Angus" .*.  Cow .*.  Price 75.5 .*. emptyTIP
    animalKey :: (HOccurs Key l, SubType l (TIP Animal)) => l -> Key
    animalKey = hOccurs
:}

-}

{- $sessionlog
[@Session log@]

>>> :t myTipyCow
myTipyCow
  :: TIP
       '[Tagged Key Key, Tagged Name Name, Tagged Breed Breed,
         Tagged Price Price]

>>> hOccurs myTipyCow :: Breed
Cow

>>> BSE .*. myTipyCow
TIPH[BSE,Key 42,Name "Angus",Cow,Price 75.5]



>>> Sheep .*. hDeleteAtLabel (Label::Label Breed) myTipyCow
TIPH[Sheep,Key 42,Name "Angus",Price 75.5]

>>> tipyUpdate Sheep myTipyCow
TIPH[Key 42,Name "Angus",Sheep,Price 75.5]


>>> tipyProject2 (Proxy :: Labels '[Name,Price]) myTipyCow
(TIPH[Name "Angus",Price 75.5],TIPH[Key 42,Cow])

>>> tipyProject (Proxy :: Labels '[Name,Price]) myTipyCow
TIPH[Name "Angus",Price 75.5]

-}


{- $sessionlog2

Don't bother repeating the type error:


>>> Sheep .*. myTipyCow
...
...No instance for (Fail (DuplicatedLabel (Label Breed)))
...

-}
