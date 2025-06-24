{-# LANGUAGE CPP #-}

{- |
   Description: Variants, i.e., labelled sums, generalizations of Either

   The HList library

   See <Data-HList-CommonMain.html#t:Variant CommonMain#Variant>
   for the public (safe) interface.

   The implementation here follows "Data.Dynamic", though Typeable is not
   needed.

   See @broken/VariantP.hs@ and @broken/VariantOld.hs@ for different approaches
   to open sums.
-}

module Data.HList.Variant where

import Data.HList.FakePrelude
import Data.HList.Record
import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.HOccurs()
import Data.HList.HArray

import Text.ParserCombinators.ReadP hiding (optional)

import Unsafe.Coerce
import GHC.Exts (Constraint)
#if __GLASGOW_HASKELL__ <= 906
import Data.Semigroup (Semigroup( .. ))
#endif
import Data.Data
import Control.Applicative
import LensDefs
import Control.Monad

-- * Labels for doctests

{- $setup

>>> import Data.HList.RecordPuns
>>> let x = Label :: Label "x"
>>> let y = Label :: Label "y"
>>> let z = Label :: Label "z"
>>> let _left = Label :: Label "left"
>>> let _right = Label :: Label "right"

>>> :set -XQuasiQuotes -XViewPatterns -XDataKinds


-- * Creating Variants

It is necessary to specify the order in which the fields occur, using
a data type like

>>> let p = Proxy :: Proxy '[Tagged "left" Char, Tagged "right" Int]

Then this argument can be passed into 'mkVariant'

>>> let v = mkVariant _left 'x' p
>>> let w = mkVariant _right 5  p

>>> :t v
v :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w
w :: Variant '[Tagged "left" Char, Tagged "right" Int]


>>> [v,w]
[V{left='x'},V{right=5}]

-}


-- ** Alternative: a 'Record' as the Proxy
{- $mkVariant2

The type of mkVariant also allows using a 'Record' as the proxy. For example:

>>> :{
let p2 = [pun| left right |] where
            left = 'a'
            right = (4::Int)
:}

>>> let v2 = mkVariant _left 'x' p2
>>> let w2 = mkVariant _right 5  p2

>>> :t v2
v2 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w2
w2 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> (v2,w2)
(V{left='x'},V{right=5})

-}

-- ** A polymorphic Proxy
{- $mkVariant3

It is also possible to leave the @Char@ and @Int@ as type variables,
and have them inferred.

>>> let p3 = Proxy :: Proxy '[Tagged "left" a, Tagged "right" b]

Using @p3@ takes some care. The following attempt shows the problem:

>>> :{
let v3' = mkVariant _left 'x' p3
    w3' = mkVariant _right (5::Int) p3
:}

>>> :t v3'
v3' :: Variant '[Tagged "left" Char, Tagged "right" b]

>>> :t w3'
w3' :: Variant '[Tagged "left" a, Tagged "right" Int]

Here each use of @p3@ does not constrain the type of the other use.
In some cases those type variables will be inferred from other constraints,
such as when putting the variants into a list

>>> [v3', w3']
[V{left='x'},V{right=5}]

In other cases the other tags will be defaulted to (), at least if `ExtendedDefaultRules` is enabled:

>>> v3'
V{left='x'}

>>> :set -XNoExtendedDefaultRules
>>> v3'
...
...No instance for (Show ...) arising from a use of ‘print’
...


Another way around this issue is to make sure that the proxy
is bound in a monomorphic pattern. These are patterns that allow
name shadowing.

* @\p -> ...@
* @case e of p -> ...@
* @do p <- e; ...@
* implicit parameters @let ?p = e in ...@
* <http://stackoverflow.com/questions/23899279#23899611 other patterns involved in mutually recursive bindings>

An example of the case:

>>> :{
let (v3,w3) = case p3 of
              p -> (mkVariant _left 'x' p,
                    mkVariant _right (5 :: Int) p)
:}


>>> :t v3
v3 :: Variant '[Tagged "left" Char, Tagged "right" Int]

>>> :t w3
w3 :: Variant '[Tagged "left" Char, Tagged "right" Int]

-}

-- --------------------------------------------------------------------------
{- |
@Variant vs@ has an implementation similar to 'Dynamic', except the
contained value is one of the elements of the @vs@ list, rather than
being one particular instance of 'Typeable'.

>>> v .!. _right
Nothing

>>> v .!. _left
Just 'x'

In some cases the 'pun' quasiquote works with variants,

>>> let f [pun| left right |] = (left,right)
>>> f v
(Just 'x',Nothing)

>>> f w
(Nothing,Just 5)


>>> let add1 v = hMapV (Fun succ :: Fun '[Enum] '()) v

>>> f (add1 v)
(Just 'y',Nothing)

>>> f (add1 w)
(Nothing,Just 6)


-}
data Variant (vs :: [*]) = Variant !Int Any

#if __GLASGOW_HASKELL__ > 707
-- the inferred role is phantom, which is not safe
type role Variant representational
#endif



-- ** Unsafe operations

-- | This is only safe if the n'th element of vs has type @Tagged t v@
unsafeMkVariant :: Int -- ^ n
                -> v
                -> Variant vs
unsafeMkVariant n a = Variant n (unsafeCoerce a)

{- | Safe when (e ~ e') given that

> Tagged t e ~ HLookupByHNatR n v
> Tagged t' e' ~ HLookupByHNatR n v'

'hUpdateAtLabel' is the safe version

-}
unsafeCastVariant :: Variant v -> Variant v'
unsafeCastVariant (Variant n e) = Variant n e

-- | in ghc>=7.8, 'Data.Coerce.coerce' is probably a better choice
castVariant :: (RecordValuesR v ~ RecordValuesR v',
              SameLength v v') => Variant v -> Variant v'
castVariant = unsafeCastVariant

instance Relabeled Variant where
    relabeled = iso castVariant castVariant

-- | private destructor. This is safe only if the value
-- contained actually has type `e`
unsafeUnVariant :: Variant v -> e
unsafeUnVariant (Variant _ e) = unsafeCoerce e


{- | This function is unsafe because it can lead to a runtime error
when used together with the 'HExtend' instance (.*.)

>>> print $ (Label :: Label "x") .=. (Nothing :: Maybe ()) .*. unsafeEmptyVariant
V{*** Exception: invalid variant

use 'mkVariant1' instead

-}
unsafeEmptyVariant :: Variant '[]
unsafeEmptyVariant = unsafeMkVariant 0 ()

-- --------------------------------------------------------------------------
-- * Public constructor

class HasField x (Variant vs) (Maybe v) =>
      MkVariant x v vs | x vs -> v where
    mkVariant :: Label x -- ^ the tag
        -> v -- ^ value to be stored
        -> proxy vs -- ^ a helper to fix the ordering and types of the
                    -- potential values that this variant contains.
                    -- Typically this will be a 'Proxy', 'Record' or
                    -- another 'Variant'
        -> Variant vs

mkVariant1 l v = l .=. Just v .*. unsafeEmptyVariant

instance (HFindLabel x vs n,
          HNat2Integral n,
          HasField x (Variant vs) (Maybe v)) =>
    MkVariant x v vs where
  mkVariant _x y _p = unsafeMkVariant (hNat2Integral (Proxy :: Proxy n)) y
  -- done as a one-instance class instead of a function to be able to hide
  -- the 'n' type variable

-- --------------------------------------------------------------------------
-- * Public destructor

{- $note 'hLookupByLabel' (synonym '.!.')

> (.!.)             :: Variant v -> Label x -> Maybe e
> hLookupByLabel    :: Label x -> Variant v -> Maybe e

'hPrism' and 'hLens'' combine this with 'mkVariant'
-}
instance (HasField x (Record vs) a,
          HFindLabel x vs n,
          HNat2Integral n)
  => HasField x (Variant vs) (Maybe a) where
  hLookupByLabel _x (Variant n d)
          | hNat2Integral (Proxy :: Proxy n) == n = Just (unsafeCoerce d)
          | otherwise = Nothing

splitVariant1 :: Variant (Tagged s x ': xs) -> Either x (Variant xs)
splitVariant1 (Variant 0 x) = Left (unsafeCoerce x)
splitVariant1 (Variant n x) = Right (Variant (n-1) x)

-- | x ~ Tagged s t
splitVariant1' :: Variant (x ': xs) -> Either x (Variant xs)
splitVariant1' (Variant 0 x) = Left (unsafeCoerce x)
splitVariant1' (Variant n x) = Right (Variant (n-1) x)

extendVariant :: Variant l -> Variant (e ': l)
extendVariant (Variant m e) = Variant (m+1) e

-- --------------------------------------------------------------------------
-- * Prism

{- | Make a @Prism (Variant s) (Variant t) a b@ out of a Label.

See "Data.HList.Labelable".'hLens'' is a more overloaded version.

Few type annotations are necessary because of the restriction
that `s` and `t` have the same labels in the same order, and to
get \"t\" the \"a\" in \"s\" is replaced with \"b\".

-}
class (SameLength s t, SameLabels s t)
        => HPrism x s t a b
          | x s -> a, x t -> b,    -- lookup
            x s b -> t, x t a -> s -- update
  where
    hPrism :: (Choice p, Applicative f)
        => Label x -> p a (f b) -> p (Variant s) (f (Variant t))


instance (
    MkVariant x b t,

    HasField x (Variant s) (Maybe a),

    -- labels in the HList are not changed at all:
    -- number, ordering, actual values are all constant
    SameLength s t,
    SameLabels s t,

    -- only the target of the prism can have it's type changed
    H2ProjectByLabels '[Label x] s si so,
    H2ProjectByLabels '[Label x] t ti to,
    so ~ to,

    -- to convince GHC the fundeps are satisfied
    HUpdateAtLabel Variant x b s t,
    HUpdateAtLabel Variant x a t s
   ) => HPrism x s t a b where
    hPrism x = prism (\b -> mkVariant x b Proxy)
                  (\s -> case hLookupByLabel x s of
                    Just a -> Right a
                    Nothing -> Left (unsafeCastVariant s :: Variant t))



-- --------------------------------------------------------------------------
-- * Read
-- | Variants are not opaque
instance (ShowVariant vs) => Show (Variant vs) where
    showsPrec _ v = ("V{"++) . showVariant v . ('}':)


-- | helper class for defining the Show instance
class ShowVariant vs where
    showVariant :: Variant vs -> ShowS

instance (ShowLabel l, Show v, ShowVariant (w ': ws))
      => ShowVariant (Tagged l v ': w ': ws) where
    showVariant vs = case splitVariant1 vs of
        Left v -> \rest -> showLabel l ++ "=" ++ show v ++ rest
        Right wws -> showVariant wws
      where l = Label :: Label l

instance (ShowLabel l, Show v, lv ~ Tagged l v) => ShowVariant '[lv] where
    showVariant vs = case splitVariant1 vs of
        Left v -> \rest -> showLabel l ++ "=" ++ show v ++ rest
        Right _ -> error "invalid variant"
      where l = Label :: Label l

-- --------------------------------------------------------------------------
-- * Show
-- | A corresponding read instance

instance ReadVariant v => Read (Variant v) where
    readsPrec _ = readP_to_S $ do
      _ <- string "V{"
      r <- readVariant
      _ <- string "}"
      return r

class ReadVariant vs where
    readVariant :: ReadP (Variant vs)

instance ReadVariant '[] where
    readVariant = return unsafeEmptyVariant

instance (ShowLabel l, Read v, ReadVariant vs,
          HOccursNot (Label l) (LabelsOf vs))
    => ReadVariant (Tagged l v ': vs) where
    readVariant = do
      mlv <- optional lv
      case mlv of
        Nothing -> do
          rest <- readVariant
          return (l .=. mlv .*. rest)
        Just e -> do
          return (mkVariant l e p)

      where
        lv = do
            _ <- string (showLabel l)
            _ <- string "="
            readS_to_P reads

        l = Label :: Label l

        p = Proxy :: Proxy (Tagged l v ': vs)


-- * Data
instance (Typeable (Variant v), GfoldlVariant v v,
          GunfoldVariant v v,
          VariantConstrs v)
        => Data (Variant v) where
    gfoldl = gfoldlVariant
    gunfold k z c = gunfoldVariant (\con -> k (z con)) (Proxy :: Proxy v) (constrIndex c - 1)
    toConstr v@(Variant n _) = case drop n (variantConstrs (dataTypeOf v) v) of
        c : _ -> c
        _ -> error "Data.HList.Variant.toConstr impossible"
    dataTypeOf x = let self = mkDataType (show (typeOf x)) (variantConstrs self x)
          in self

class VariantConstrs (xs :: [*]) where
  variantConstrs :: DataType -> proxy xs -> [Constr]

instance VariantConstrs '[] where
  variantConstrs _ _ = []

instance (ShowLabel l, VariantConstrs xs) => VariantConstrs (Tagged l e ': xs) where
  variantConstrs dt _ = mkConstr dt (showLabel (Label :: Label l)) [] Prefix :
        variantConstrs dt (Proxy :: Proxy xs)




{- | [@implementation of gunfold for Variant@]

In ghci

> :set -ddump-deriv -XDeriveDataTypeable
> data X a b c = A a | B b | C c deriving (Data,Typeable)

shows that gunfold is defined something like

> gunfold k z c = case constrIndex c of
>       1 -> k (z Ghci1.A)
>       2 -> k (z Ghci1.B)
>       _ -> k (z Ghci1.C)

If we instead had

> type X a b c = Variant [Tagged "A" a, Tagged "B" b, Tagged "C" c]

Then we could write:

> gunfold1 :: (forall b r. Data b => (b -> r) -> c r)
>          -> Variant [Tagged "A" a, Tagged "B" b, Tagged "C" c]
> gunfold1 f c = case constrIndex c of
>       1 -> f mkA
>       2 -> f mkB
>       _ -> f mkC
>   where mkA a = mkVariant (Label :: Label "A") (a :: a) v
>         mkB b = mkVariant (Label :: Label "B") (b :: b) v
>         mkC c = mkVariant (Label :: Label "C") (c :: c) v
>         v = Proxy :: Proxy [Tagged "A" a, Tagged "B" b, Tagged "C" c]

where @f = k.z@


-}
class GunfoldVariant (es :: [*]) v where
    gunfoldVariant ::
        (forall b. Data b => (b -> Variant v) -> c (Variant v))
          -- ^ @f = k . z@
        -> Proxy es
        -> Int
        -> c (Variant v)

instance (MkVariant l e v, Data e) => GunfoldVariant '[Tagged l e] v where
    gunfoldVariant f _ _ = f (\e -> mkVariant (Label :: Label l) (e :: e) Proxy)

instance (MkVariant l e v, Data e,
        GunfoldVariant (b ': bs) v) => GunfoldVariant (Tagged l e ': b ': bs)  v where
    gunfoldVariant f _ 0 = f (\e -> mkVariant (Label :: Label l) (e :: e) Proxy)
    gunfoldVariant f _ n = gunfoldVariant f (Proxy :: Proxy (b ': bs)) (n-1)



class GfoldlVariant xs xs' where
  -- | the same as 'gfoldl', except the variant that is returned can have more
  -- possible values (needed to actually implement gfoldl).
  gfoldlVariant ::
     (forall d b. Data d => c (d -> b) -> d -> c b)
     -> (forall g. g -> c g) -> Variant xs -> c (Variant xs')

instance (a ~ Tagged l v, MkVariant l v r, Data v,
          GfoldlVariant (b ': c) r)
      => GfoldlVariant (a ': b ': c) r where
  gfoldlVariant k z xxs = case splitVariant1 xxs of
      Right xs -> gfoldlVariant k z xs
      -- If the c@type variable in 'gfoldl' had a Functor constraint,
      -- this case could be extendVariant `fmap` gfoldl k z xs,
      -- and then 'GfoldlVariant' would be unnecessary
      Left x ->
            let mkV e = mkVariant (Label :: Label l) e Proxy
            in z mkV `k` x

instance (Unvariant '[a] v, a ~ Tagged l v, Data v,
          MkVariant l v b) => GfoldlVariant '[a] b where
    gfoldlVariant k z xxs = z mkV `k` unvariant xxs
        where mkV e = mkVariant (Label :: Label l) e Proxy



-- --------------------------------------------------------------------------
-- * Map
-- | Apply a function to all possible elements of the variant
newtype HMapV f = HMapV f

-- | shortcut for @applyAB . HMapV@. 'hMap' is more general
hMapV f v = applyAB (HMapV f) v

-- | @hMapOutV f = unvariant . hMapV f@, except an ambiguous type
-- variable is resolved by 'HMapOutV_gety'
hMapOutV :: forall x y z f. (SameLength x y,
      HMapAux Variant (HFmap f) x y,
      Unvariant y z,
      HMapOutV_gety x z ~ y
  ) => f -> Variant x -> z
hMapOutV f v = unvariant (hMapV f v :: Variant y)


-- | resolves an ambiguous type in 'hMapOutV'
type family HMapOutV_gety (x :: [*]) (z :: *) :: [*]
type instance HMapOutV_gety (Tagged s x ': xs) z = Tagged s z ': HMapOutV_gety xs z
type instance HMapOutV_gety '[] z = '[]


-- | apply a function to all values that could be in the variant.
instance (vx ~ Variant x,
          vy ~ Variant y,
          HMapAux Variant (HFmap f) x y,
          SameLength x y)
     => ApplyAB (HMapV f) vx vy where
    applyAB (HMapV f) x = hMapAux (HFmap f) x

instance (ApplyAB f te te') => HMapAux Variant f '[te] '[te'] where
    hMapAux f v = case splitVariant1' v of
        Left te -> unsafeMkVariant 0 (applyAB f te :: te')
        Right _ -> error "HMapVAux: variant invariant broken"

instance (ApplyAB f te te',
          HMapCxt Variant f (l ': ls) (l' ': ls'))
    => HMapAux Variant f (te ': l ': ls) (te' ': l' ': ls') where
      hMapAux f v = case splitVariant1' v of
          Left te -> unsafeMkVariant 0 (applyAB f te :: te')
          Right es -> extendVariant (hMapAux f es)

-- --------------------------------------------------------------------------
-- * HUpdateAtLabel instance

{- |

> hUpdateAtLabel x e' (mkVariant x e proxy) == mkVariant x e' proxy
> hUpdateAtLabel y e' (mkVariant x e proxy) == mkVariant x e  proxy

-}
instance
   (HUpdateVariantAtLabelCxt l e v v' n _e) =>
    HUpdateAtLabel Variant l e v v' where
    hUpdateAtLabel l e v = case hLookupByLabel l v of
          Just _e -> mkVariant l e (Proxy :: Proxy v')
          Nothing -> unsafeCastVariant v

type HUpdateVariantAtLabelCxt l e v v' n _e =
   (HFindLabel l v n,
    HFindLabel l v' n,
    HUpdateAtHNatR n (Tagged l e) v ~ v',
    HasField l (Variant v) (Maybe _e),
    HasField l (Record v') e,
    MkVariant l e v')


-- --------------------------------------------------------------------------
-- * HExtend instance
{- | Extension for Variants prefers the first value

> (l .=. Nothing) .*. v = v
> (l .=. Just e)  .*. _ = mkVariant l e Proxy

-}
instance (le ~ Tagged l (Maybe e), HOccursNot (Label l) (LabelsOf v)) =>
    HExtend le (Variant v) where
    type HExtendR le (Variant v) = Variant (UnMaybe le ': v)
    Tagged (Just e) .*. _ = unsafeMkVariant 0 e
    Tagged Nothing .*. (Variant n e) = Variant (n+1) e

type family UnMaybe le
type instance UnMaybe (Tagged l (Maybe e)) = Tagged l e

-- | used for 'HExtend' 'TIP'
type instance UnMaybe (Maybe e) = e


-- --------------------------------------------------------------------------
-- * Conversion to an untagged value
class HAllEqVal (x :: [*]) (b :: Bool) | x -> b
instance HAllEqVal '[] True
instance HAllEqVal '[x] True
instance (HEq a a' b,
          HAllEqVal (Tagged t a' ': xs) b2,
          HAnd b b2 ~ b3) =>
  HAllEqVal (Tagged s a ': Tagged t a' ': xs) b3


class HAllEqVal' (x :: [*])
instance HAllEqVal' '[]
instance HAllEqVal' '[x]
instance (HAllEqVal' (ta ': xs),
          a' ~ a,
          ta ~ Tagged t a,
          ta' ~ Tagged t' a')
  => HAllEqVal' (ta' ': ta ': xs)


{- | Similar to 'unvariant', except type variables in @v@
will be made equal to @e@ if possible. That allows the type
of @Nothing@ to be inferred as @Maybe Char@.

>>> unvariant' $ x .=. Nothing .*. mkVariant1 y 'y'
'y'

However, this difference leads to more local error messages
(@Couldn't match type ‘()’ with ‘Char’@), rather than the following
with @unvariant@:

> Fail
>    '("Variant",
>      '[Tagged "left" Char, Tagged "right" ()],
>      "must have all values equal to ",
>      e))

-}
class Unvariant' v e | v -> e where
    unvariant' :: Variant v -> e

instance (HAllEqVal' (Tagged () e ': v), Unvariant v e) =>
    Unvariant' v e where
  unvariant' = unvariant

{- | Convert a Variant which has all possibilities having the same type
into a value of that type. Analogous to @either id id@.

See also 'unvariant'' -}
class Unvariant v e | v -> e where
    unvariant :: Variant v -> e

instance (Unvariant1 b v e,
          HAllEqVal v b,
          HAllEqVal (Tagged () e ': v) b)
    => Unvariant v e where
      unvariant = unvariant1 (Proxy :: Proxy b)


class Unvariant1 b v e | b v -> e where
    unvariant1 :: Proxy b -> Variant v -> e

instance (v ~ Tagged t1 e)
    => Unvariant1 True (v ': vs) e where
    unvariant1 _ = unsafeUnVariant

data UnvariantTypeMismatch (vs :: [*])

instance Fail (UnvariantTypeMismatch (v ': vs))
      => Unvariant1 False (v ': vs) (UnvariantTypeMismatch (v ': vs)) where
    unvariant1 _ = error "Data.HList.Variant.Unvariant1 Fail must have no instances"

instance Fail "Unvariant applied to empty variant"
      => Unvariant1 b '[] (Proxy "Unvariant applied to empty variant") where
    unvariant1 _ = error "Data.HList.Variant.Unvariant1 Fail must have no instances"

{- | @Lens (Variant s) (Variant t) a b@

Analogue of @Control.Lens.chosen :: Lens (Either a a) (Either b b) a b@
-}
unvarianted :: (Unvariant' s a,
                Unvariant' t b,
                SameLabels s t, -- extra constraints to reduce ambiguity
                SameLength s t,
                Functor f) =>
    (a -> f b) -> Variant s -> f (Variant t)
unvarianted f v@(Variant n _) = fmap (\e' -> unsafeMkVariant n e')
                                      (f (unvariant' v))

-- | @Lens' (Variant s) a@
--
-- where we might have @s ~ '[Tagged t1 a, Tagged t2 a]@
unvarianted' x = simple (unvarianted x)

-- * Zip

{- | Applies to variants that have the same labels
in the same order. A generalization of

> zipEither :: Either a b -> Either a b -> Maybe (Either (a,a) (b,b))
> zipEither (Left a) (Left a') = Just (Left (a,a'))
> zipEither (Right a) (Right a') = Just (Right (a,a'))
> zipEither _ _ = Nothing

see 'HZip' for zipping other collections

-}
class ZipVariant x y xy | x y -> xy, xy -> x y where
    zipVariant :: Variant x -> Variant y -> Maybe (Variant xy)

instance ZipVariant '[] '[] '[] where
    zipVariant _ _ = Nothing

instance (tx ~ Tagged t x,
          ty ~ Tagged t y,
          txy ~ Tagged t (x,y),
          ZipVariant xs ys zs,
          MkVariant t (x,y) (txy ': zs))
  => ZipVariant (tx ': xs) (ty ': ys) (txy ': zs) where
    zipVariant x y = case (splitVariant1 x, splitVariant1 y) of
        (Left x', Left y') -> Just (mkVariant (Label :: Label t) (x',y') Proxy)
        (Right x', Right y') -> extendVariant <$> zipVariant x' y'
        _ -> Nothing


instance (HUnzip Variant (x2 ': xs) (y2 ': ys) (xy2 ': xys),
          SameLength xs ys,
          SameLength ys xys,
          tx ~ Tagged t x,
          ty ~ Tagged t y,
          txy ~ Tagged t (x,y))
      => HUnzip Variant (tx ': x2 ': xs) (ty ': y2 ': ys) (txy ': xy2 ': xys) where
    hUnzip xy = case splitVariant1 xy of
      Left (x,y) -> (mkVariant (Label :: Label t) x Proxy,
                     mkVariant (Label :: Label t) y Proxy)
      Right xy' | (x,y) <- hUnzip xy' ->
                    (extendVariant x,
                     extendVariant y)

instance (Unvariant '[txy] txy,
          tx ~ Tagged t x,
          ty ~ Tagged t y,
          txy ~ Tagged t (x,y))
      => HUnzip Variant '[tx] '[ty] '[txy] where
    hUnzip xy | Tagged (x,y) <- unvariant xy =
        (mkVariant1 Label x, mkVariant1 Label y)


-- ** with a record

{- | Apply a record of functions to a variant of values.
The functions are selected based on those having the same
label as the value.

-}
class (SameLength v v',
       SameLabels v v')  => ZipVR fs v v' | fs v -> v' where
    -- | 'zipVR' is probably a better choice in most
    -- situations, since it requires that @fs@ has one function for every
    -- element of @v@
    zipVR_ :: Record fs -> Variant v -> Variant v'

instance (lv ~ Tagged l v,
          lv' ~ Tagged l v',
          HMemberM (Label l) (LabelsOf fs) b,
          HasFieldM l (Record fs) f,
          DemoteMaybe (v -> v) f ~ (v -> v'),
          MkVariant l v' (lv' ': rs),
          ZipVR fs vs rs) =>
          ZipVR fs (lv ': vs) (lv' ': rs) where
    zipVR_ r lvs = case splitVariant1 lvs of
                  Left v | v' <- hLookupByLabelM l r (id :: v -> v) v -> mkVariant l v' Proxy
                  Right vs -> extendVariant $ zipVR_ r vs
      where l = Label :: Label l


instance ZipVR fs '[] '[] where
    zipVR_ _ x = x

{- |

>>> let xy = x .*. y .*. emptyProxy
>>> let p = Proxy `asLabelsOf` xy
>>> let vs = [ mkVariant x 1.0 p, mkVariant y () p ]


>>> zipVR (hBuild (+1) id) `map` vs
[V{x=2.0},V{y=()}]


-}
zipVR :: (SameLabels fs v, SameLength fs v, ZipVR fs v v',
          ZipVRCxt fs v v')
    => Record fs -> Variant v -> Variant v'
zipVR = zipVR_


{- | Lets 'zipVR' act as if @'ZipVR' fs v v'@ had an FD @v v' -> fs@

> ZipVRCxt [Tagged s f,  Tagged t g]
>          [Tagged s fx, Tagged t gx]
>          [Tagged s fy, Tagged t gy]
>   = (f ~ (fx -> fy), g ~ (gx -> gy))

-}
type family ZipVRCxt (fs :: [*]) (xs :: [*]) (ys :: [*]) :: Constraint

type instance ZipVRCxt (Tagged s f ': fs) (Tagged s x ': xs) (Tagged s y ': ys) =
        (f ~ (x -> y), ZipVRCxt fs xs ys)
type instance ZipVRCxt '[] '[] '[] = ()

-- * Eq
instance Eq (Variant '[]) where
  _ == _ = True

instance (Eq (Variant xs), Eq x) => Eq (Variant (x ': xs)) where
  v == v' = case (splitVariant1' v, splitVariant1' v') of
    (Left l, Left r) -> l == r
    (Right l, Right r) -> l == r
    _ -> False

-- ** Alternative Eq
-- | implemented like @and (zipWith (==) xs ys)@. Behaves the same as the Eq instances for 'Variant'
eqVariant v v' = maybe False (hMapOutV UncurryEq) $ zipVariant v v'

data UncurryEq = UncurryEq

instance (ee ~ (e,e), Eq e, bool ~ Bool) =>
    ApplyAB UncurryEq ee bool where
      applyAB _ (e,e') = e == e'

-- * Ord
instance Ord (Variant '[]) where
  compare _ _ = EQ

instance (Ord x, Ord (Variant xs)) => Ord (Variant (x ': xs)) where
  compare a b = compare (splitVariant1' a) (splitVariant1' b)

-- * Bounded
instance (Bounded x, Bounded z,
          HRevAppR (Tagged s x ': xs) '[] ~ (Tagged t z ': sx),
          MkVariant t z (Tagged s x ': xs))
        => Bounded (Variant (Tagged s x ': xs)) where
  minBound = mkVariant (Label :: Label s) (minBound :: x) Proxy
  maxBound = mkVariant (Label :: Label t) (maxBound :: z) Proxy

-- * Enum
{- |

>>> let t = minBound :: Variant '[Tagged "x" Bool, Tagged "y" Bool]
>>> [t .. maxBound]
[V{x=False},V{x=True},V{y=False},V{y=True}]


[@Odd behavior@]
There are some arguments that this instance should not exist.

The last type in the Variant does not need to be Bounded. This
means that 'enumFrom' behaves a bit unexpectedly:

>>> [False .. ]
[False,True]

>>> [t .. ]
[V{x=False},V{x=True},V{y=False},V{y=True},V{y=*** Exception: Prelude.Enum.Bool.toEnum: bad argument

This is a \"feature\" because it allows an @Enum (Variant '[Tagged \"a\" Bool, Tagged \"n\" 'Integer'])@

Another difficult choice is that the lower bound is @fromEnum 0@ rather than @minBound@:

>>> take 5 [ minBound :: Variant '[Tagged "b" Bool, Tagged "i" Int] .. ]
[V{b=False},V{b=True},V{i=0},V{i=1},V{i=2}]

-}
instance (Enum x, Bounded x, Enum (Variant (y ': z))) => Enum (Variant (Tagged s x ': y ': z)) where
  fromEnum v = case splitVariant1 v of
    Left x -> fromEnum x
    Right yz -> 1 + fromEnum (maxBound :: Tagged s x) + fromEnum yz

  toEnum n
      | m >= n = mkVariant (Label :: Label s) (toEnum n) Proxy
      | otherwise = extendVariant $ toEnum (n - m - 1)
    where m = fromEnum (maxBound :: Tagged s x)

{- |

While the instances could be written Enum (Variant '[])
Eq/Ord which cannot produce values, so they have instances for
empty variants ('unsafeEmptyVariant'). Enum can produce values,
so it is better that @fromEnum 0 :: Variant '[]@ fails with No instance for
@Enum (Variant '[])@ than producing an invalid variant.

-}
instance Enum x => Enum (Variant '[Tagged s x]) where
  fromEnum v = case splitVariant1 v of
    Left x -> fromEnum x
    _ -> error "Data.HList.Variant fromEnum impossible"
  toEnum n = mkVariant (Label :: Label s) (toEnum n) Proxy

-- * Ix (TODO)

-- * Semigroup
instance (Unvariant '[Tagged t x] x, Semigroup x) => Semigroup (Variant '[Tagged t x]) where
    a <> b = case (unvariant a, unvariant b) of
                    (l, r) -> mkVariant (Label :: Label t) (l <> r) Proxy

instance (Semigroup x, Semigroup (Variant (a ': b))) => Semigroup (Variant (Tagged t x ': a ': b)) where
    a <> b = case (splitVariant1 a, splitVariant1 b) of
                    (Left l, Left r) -> mkVariant (Label :: Label t) (l <> r) Proxy
                    (Left l, _) -> mkVariant (Label :: Label t) l Proxy
                    (_, Left r) -> mkVariant (Label :: Label t) r Proxy
                    (Right l, Right r) -> extendVariant $ l <> r

-- * Monoid
instance (Unvariant '[Tagged t x] x, Monoid x) => Monoid (Variant '[Tagged t x]) where
    mempty = mkVariant (Label :: Label t) mempty Proxy
#if __GLASGOW_HASKELL__ <= 906
    mappend a b = case (unvariant a, unvariant b) of
                    (l, r) -> mkVariant (Label :: Label t) (mappend l r) Proxy
#endif


instance (Monoid x, Monoid (Variant (a ': b))) => Monoid (Variant (Tagged t x ': a ': b)) where
    mempty = extendVariant mempty
#if __GLASGOW_HASKELL__ <= 906
    mappend a b = case (splitVariant1 a, splitVariant1 b) of
                    (Left l, Left r) -> mkVariant (Label :: Label t) (mappend l r) Proxy
                    (Left l, _) -> mkVariant (Label :: Label t) l Proxy
                    (_, Left r) -> mkVariant (Label :: Label t) r Proxy
                    (Right l, Right r) -> extendVariant $ mappend l r
#endif

-- * Projection

{- | convert a variant with more fields into one with fewer (or the same)
fields.


>>> let ty = Proxy :: Proxy [Tagged "left" Int, Tagged "right" Int]
>>> let l = mkVariant _left 1 ty
>>> let r = mkVariant _right 2 ty


>>> map projectVariant [l, r] :: [Maybe (Variant '[Tagged "left" Int])]
[Just V{left=1},Nothing]


@'rearrangeVariant' = 'fromJust' . 'projectVariant'@ is one implementation
of 'rearrangeVariant', since the result can have the same fields with
a different order:

>>> let yt = Proxy :: Proxy [Tagged "right" Int, Tagged "left" Int]

>>> map projectVariant [l, r] `asTypeOf` [Just (mkVariant _left 0 yt)]
[Just V{left=1},Just V{right=2}]


-}
class ProjectVariant x y where
    projectVariant :: Variant x -> Maybe (Variant y)

instance (ProjectVariant x ys,
          HasField t (Variant x) (Maybe y),
          HOccursNot (Label t) (LabelsOf ys),
          ty ~ Tagged t y)
  => ProjectVariant x (ty ': ys) where
    projectVariant x = y `mplus` ys
      where t = Label :: Label t
            y = (\v -> mkVariant t v Proxy) <$> x .!. t
            ys = (mty  .*.) <$> (projectVariant x :: Maybe (Variant ys))
            mty = Tagged Nothing :: Tagged t (Maybe y)

instance ProjectVariant x '[] where
    projectVariant _ = Nothing



{- | @projectExtendVariant = fmap 'extendVariant' . 'projectVariant'@

where intermediate variant is as large as possible. Used to implement
"Data.HList.Labelable".'projected'

Note that:

>>> let r = projectExtendVariant (mkVariant1 Label 1 :: Variant '[Tagged "x" Int])
>>> r :: Maybe (Variant '[Tagged "x" Integer])
Nothing

-}
class HAllTaggedLV y => ProjectExtendVariant x y where
    projectExtendVariant :: Variant x -> Maybe (Variant y)

instance HAllTaggedLV y => ProjectExtendVariant '[] y where
    projectExtendVariant _ = Nothing

instance (lv ~ Tagged l v,
          HMemberM lv y inY,
          ProjectExtendVariant' inY lv y,
          ProjectExtendVariant xs y
      ) => ProjectExtendVariant (lv ': xs) y where
  projectExtendVariant v = case splitVariant1' v of
      Left lv -> projectExtendVariant' (Proxy :: Proxy inY) lv
      Right v' -> projectExtendVariant v'


class ProjectExtendVariant' (inY :: Maybe [*]) lv (y :: [*]) where
    projectExtendVariant' :: Proxy inY -> lv -> Maybe (Variant y)

instance ProjectExtendVariant' Nothing lv y where
    projectExtendVariant' _ _ = Nothing

instance (MkVariant l v y, lv ~ Tagged l v) => ProjectExtendVariant' (Just t) lv y where
    projectExtendVariant' _ (Tagged v) = Just (mkVariant (Label :: Label l) v Proxy)



class (ProjectVariant x yin,
       ProjectVariant x yout) => SplitVariant x yin yout where
    splitVariant :: Variant x -> Either (Variant yin) (Variant yout)

instance
   (-- implementation
    ProjectVariant x yin,
    ProjectVariant x yout,

    -- constraints to ensure exactly one of
    -- the uses of projectVariant gives a Just
    H2ProjectByLabels (LabelsOf yin) x xi xo,
    HRearrange (LabelsOf yin) xi yin,
    HRearrange (LabelsOf yout) xo yout,

    HLeftUnion xi xo xixo,
    HRearrange (LabelsOf x) xixo x,

    -- probably redundant
    HAllTaggedLV x, HAllTaggedLV yin, HAllTaggedLV yout) =>
  SplitVariant x yin yout where
  splitVariant x = case (projectVariant x, projectVariant x) of
   (Nothing, Just yout) -> Right yout
   (Just yin, Nothing) -> Left yin
   _ -> error "Data.HList.Variant:splitVariant impossible"

-- | @projectVariant . extendsVariant = Just@ (when the types match up)
--
-- 'extendVariant' is a special case
class (HAllTaggedLV y, HAllTaggedLV x) => ExtendsVariant x y where
    extendsVariant :: Variant x -> Variant y

instance (MkVariant l e y, le ~ Tagged l e,
          ExtendsVariant (b ': bs) y) => ExtendsVariant (le ': b ': bs) y where
    extendsVariant v = case splitVariant1 v of
        Left e -> mkVariant (Label :: Label l) (e :: e) Proxy
        Right vs -> extendsVariant vs

instance (HAllTaggedLV x, Unvariant '[le] e, MkVariant l e x,
          le ~ Tagged l e) => ExtendsVariant '[le] x where
    extendsVariant v = mkVariant (Label :: Label l) (unvariant v) Proxy


-- | @rearrangeVariant@ is a specialization of 'extendsVariant' whose
-- result is always . see also 'rearranged'
rearrangeVariant :: (SameLength v v', ExtendsVariant v v')
      => Variant v -> Variant v'
rearrangeVariant v = extendsVariant v

instance (SameLength s a, ExtendsVariant s a,
          SameLength b t, ExtendsVariant b t) => Rearranged Variant s t a b
  where
    rearranged = iso rearrangeVariant rearrangeVariant

-- | @Prism (Record tma) (Record tmb) (Variant ta) (Variant tb)@
--
-- see 'hMaybied''
hMaybied x = prism variantToHMaybied
    (\s -> case hMaybiedToVariants s of
          [a] -> Right a
          _ -> Left (hMapR HCastF s))
    x


data HCastF = HCastF

instance (mx ~ Maybe x,
          my ~ Maybe y,
          HCast y x) =>
  ApplyAB HCastF mx my where
    applyAB _ x = hCast =<< x



{- | @Prism' (Record tma) (Variant ta)@

where @tma@ and @tmb@ are lists like

> tma ~ '[Tagged x (Maybe a), Tagged y (Maybe b)]
> ta  ~ '[Tagged x        a , Tagged y        b ]

If one element of the record is Just, the Variant will
contain that element. Otherwise, the prism fails.

[@Note@]

The types work out to define a prism:

@l = 'prism'' 'variantToHMaybied' ('listToMaybe' . 'hMaybiedToVariants')@

but the law: @s^?l ≡ Just a ==> l # a ≡ s@ is not followed,
because we could have:

@
  s, s2 :: Record '[Tagged "x" (Maybe Int), Tagged "y" (Maybe Char)]
  s = hBuild (Just 1) (Just '2')
  s2 = hBuild (Just 1) Nothing

  v :: Variant '[Tagged "x" Int, Tagged "y" Char]
  v = mkVariant (Label :: Label "x") 1 Proxy
@

So that @s^?l == Just v@. But @l#v == s2 /= s@, while the law
requires @l#v == s@. hMaybied avoids this problem by only
producing a value when there is only one present.

-}
hMaybied' x = simple (hMaybied (simple x))

class VariantToHMaybied v r | v -> r, r -> v where
    variantToHMaybied :: Variant v -> Record r

instance VariantToHMaybied '[] '[] where
    variantToHMaybied _ = emptyRecord

instance (VariantToHMaybied v r,
          HReplicateF nr ConstTaggedNothing () r,

          tx ~ Tagged t x,
          tmx ~ Tagged t (Maybe x))
    => VariantToHMaybied (tx ': v) (tmx ': r) where
      variantToHMaybied v = case splitVariant1 v of
            Left x -> Record
                $ HCons (Tagged (Just x))
                $ hReplicateF Proxy ConstTaggedNothing ()
            Right rest ->
                case variantToHMaybied rest of
                  Record a -> Record $ (Tagged Nothing :: Tagged t (Maybe x)) `HCons` a
          -- don't use (.*.) because we have (LabelsOf v ~ LabelsOf r), so
          -- the duplicate check (HRLabelSet) implied by (.*.) is redundant

data ConstTaggedNothing = ConstTaggedNothing
instance (y ~ Tagged t (Maybe e)) => ApplyAB ConstTaggedNothing x y where
    applyAB _ _ = Tagged Nothing

-- | Every element of the record that is Just becomes one element
-- in the resulting list. See 'hMaybied'' example types that @r@
-- and @v@ can take.
hMaybiedToVariants ::
  (HFoldr HMaybiedToVariantFs [Variant '[]] r [Variant v], -- impl
   VariantToHMaybied v r -- evidence for typechecking
  ) => Record r -> [Variant v]
hMaybiedToVariants (Record r) = hFoldr HMaybiedToVariantFs ([] :: [Variant '[]]) r

data HMaybiedToVariantFs = HMaybiedToVariantFs

instance (x ~ (Tagged t (Maybe e), [Variant v]),
          y ~ [Variant (Tagged t e ': v)],
          MkVariant t e (Tagged t e ': v))
        => ApplyAB HMaybiedToVariantFs x y where

  applyAB _ (Tagged me, v) = case me of
    Just e -> mkVariant (Label :: Label t) e Proxy : map extendVariant v
    _ -> fmap extendVariant v
