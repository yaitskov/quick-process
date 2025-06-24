{-# LANGUAGE CPP #-}
#if (__GLASGOW_HASKELL__ < 709)
-- TryCollectionList needs overlap
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
{- | Description: access nested records/variants given only the last label along a path -}
module Data.HList.Dredge where

import Data.HList.Record
import Data.HList.Variant
import Data.HList.HList
import Data.HList.TIP
import Data.HList.TIC
import Data.HList.FakePrelude
import Data.HList.Labelable
import LensDefs (isSimple)
import Data.HList.TypeEqO () -- if this is missing, dredge fails


#if (__GLASGOW_HASKELL__ == 800)
-- https://ghc.haskell.org/trac/ghc/ticket/13371
toLabelx x = toLabelSym x
#else
toLabelx x = toLabel x
#endif

{- |

Using HListPP syntax for short hand, @dredge `foo@ expands out to
something like @`path . `to . `foo@, with the restriction that
there is only one possible @`path .  `to@ which leads to the
label @foo@.

For example, if we have the following definitions,

> type BVal a = Record '[Tagged "x" a, Tagged "a" Char]
> type R a = Record  [Tagged "a" Int, Tagged "b" (BVal a)]
> type V a = Variant [Tagged "a" Int, Tagged "b" (BVal a)]
> lx = Label :: Label "x"

Then we have:

> dredge `x :: Lens (R a) (R b) a b
> dredge lx :: Lens (R a) (R b) a b

> dredge `x :: Traversal (V a) (V b) a b -- there were only variants along the path we'd get a Prism
> dredge lx :: Traversal (V a) (V b) a b

[@result-type directed operations are supported@]

There are two ways to access a field with tag @a@ in the R type
defined above, but they result in fields with different types
being looked up:

> `a        :: Lens' (R a) Char
> `b . `a   :: Lens' (R a) Int

so provided that the result type is disambiguated by the context,
the following two types can happen

> dredge `a :: Lens' (R a) Char
> dredge `a :: Lens' (R a) Int


[@TIP & TIC@]

type indexed collections are allowed along those paths, but
as explained in the 'Labelable' instances, only simple optics
(Lens' / Prism' / Traversal' ) are produced. @dredgeTI'@
works better if the target is a TIP or TIC

-}
dredge label = getSAfromOutputOptic $ \ pr pa ->
      hLens'Path (labelPathEndingWithTD pr (toLabelx label) pa)



getSAfromOutputOptic :: (p a fb -> p rs rft) ~ stab
                => (Proxy (rs :: *) -> Proxy (a :: *) -> stab) -> stab
getSAfromOutputOptic f = f Proxy Proxy


-- | 'dredge' except a simple (s ~ t, a ~ b) optic is produced
dredge' label = isSimple (dredge label)


-- | dredgeND (named directed only) is the same as 'dredge', except the
-- result type (@a@) is not used when the label would otherwise
-- be ambiguous. dredgeND might give better type errors, but otherwise
-- there should be no reason to pick it over dredge
dredgeND label = getSAfromOutputOptic $ \ pr _a ->
      hLens'Path (labelPathEndingWith pr (toLabelx label))


-- | 'dredgeND' except a simple (s ~ t, a ~ b) optic is produced
dredgeND' label = isSimple (dredgeND label)


{- | The same as dredgeND', except intended for TIP/TICs because
the assumption is made that @l ~ v@ for the @Tagged l v@ elements.
In other words, ticPrism' and 'tipyLens'' could usually
be replaced by

> dredgeTI' :: _ => Label a -> Lens'  (TIP s) a
> dredgeTI' :: _ => Label a -> Prism' (TIC s) a

where we might have @s ~ '[Tagged a a, Tagged b b]@

-}
dredgeTI' label = isSimple lens where
        lens = getSAfromOutputOptic $ \ pr pa ->
            hLens'Path (labelPathEndingWith pr (pa `proxyTypeOf` label))

        proxyTypeOf :: p a -> q a -> Label a
        proxyTypeOf _ _ = Label


-- | @HSingleton msg xs x@ is like @'[x] ~ xs@ if that constraint can hold,
-- otherwise it is @Fail msg@. See comments on 'Fail' about how its kind
-- varies with ghc version.
class HSingleton (msgAmb :: m) (msgEmpty :: m2) (ns :: [k]) (p :: k) | ns -> p
instance HSingleton m1 m2 '[n] n
instance (Fail m2, Any ~ a) => HSingleton m1 m2 '[] a
instance (Fail m1, Any ~ a) => HSingleton m1 m2 (n1 ': n2 ': n3) a


-- | @HGuardNonNull msg xs@ is like @when (null xs) (fail msg)@
class HGuardNonNull emptymsg (xs :: [k])

instance Fail msg => HGuardNonNull msg '[]
instance             HGuardNonNull msg (x ': xs)


-- | @ConsTrue b x xs r@ is like @r = if b then x:xs else xs@
class ConsTrue (b :: Bool) (x :: k) (xs :: [k]) (r :: [k]) | b x xs -> r, r b -> xs, x xs r -> b
instance ConsTrue True x xs (x ': xs)
instance ConsTrue False x xs xs

-- | @FilterLastEq x xs ys ys'@ determines ys' such that it
-- contains all of the @ys !! i@ such that @last (xs !! i) == x@.
-- In other words it is like
--
-- > ys' = [ y |  (xsElt, y) <- zip xs ys, last xsElt == x ]
class FilterLastEq (x :: k) (xs :: [[k]]) (ys :: [m]) (ys' :: [m]) | x xs ys -> ys'
instance (HReverse path (y' ': rest), HEq y y' b, ConsTrue b z r1 r,
          FilterLastEq y xs zs r1) => FilterLastEq y (path ': xs) (z ': zs) r

instance FilterLastEq y '[] '[] '[]

-- | The same as 'FilterLastEq' except @id@ is used instead of @last@
class FilterVEq (v :: *) (vs :: [*]) (ns :: [k]) (ns' :: [k]) | v vs ns -> ns'

instance FilterVEq v '[] '[] '[]

instance
   (HEq v v' b,
    ConsTrue b n ns1 ns2,
    FilterVEq v vs ns ns1)
    => FilterVEq v (v' ': vs) (n ': ns) ns2

-- | like @FilterVEq@, except if there is
class FilterVEq1 (v :: *) (vs :: [*]) (ns :: [k]) (ns' :: [k]) | v vs ns -> ns'
instance (v ~ v') => FilterVEq1 v '[ v' ] ns ns
instance FilterVEq1 v '[] '[] '[]
instance FilterVEq v (a ': b ': c)  ns ns' => FilterVEq1 v (a ': b ': c) ns ns'

-- | @LabelPathEndingWith r l path@
--
-- determines a unique path suitable for 'hLookupByLabelPath'
-- (calling 'Fail' otherwise) through the
-- nested records/variants in r ending with l
class LabelPathEndingWith (r :: *) (l :: k) (path :: [*]) | r l -> path where
    labelPathEndingWith :: proxy r -> Label l -> Label path
    labelPathEndingWith _ _ = Label

instance
   (FieldTree r ns,
    FilterLastEq (Label l) ns ns ns',
    HSingleton (NonUnique' r l) (NamesDontMatch r ns l) ns' path)
    => LabelPathEndingWith r l path


labelPathEndingWithTD :: forall r l v path
                                vs vs1 ns ns1 ns2.
   (SameLength ns vs,
    SameLength ns1 vs1,
    FieldTree r ns,
    FieldTreeVal r vs,
    FilterLastEq (Label l) ns ns ns1,
    FilterLastEq (Label l) ns vs vs1,
    FilterVEq1 v vs1 ns1 ns2,

    HGuardNonNull (NamesDontMatch r ns l) ns1,

    -- '[path] ~ ns2, plus error reporting if ns2 has >1 or 0 elements
    HSingleton (NonUnique r v l) (TypesDontMatch r ns1 vs1 v) ns2 path)
    => Proxy r -> Label l -> Proxy v -> Label path
labelPathEndingWithTD _ _ _ = Label


type NamesDontMatch r ns l = ErrShowType r
  :$$: ErrText "has paths"  :<>: ErrShowType ns
  :$$: ErrText "but none which end in the desired label" :<>: ErrShowType l

type NonUnique' r l = ErrText "Path ending in label " :<>: ErrShowType l
  :$$: ErrText "is not unique in " :<>: ErrShowType r

type NonUnique r v l = NonUnique' r l
  :$$: ErrText "also considering the v type " :<>: ErrShowType v

{- | XXX

> let x = 'x'; y = [pun| x |]; z = [pun| y |]
> z & dredge (Label :: Label "x") %~ (succ :: Int -> Int)

Should reference this type error, but for whatever reason it doesn't

-}
type TypesDontMatch r ns1 vs1 v = ErrShowType r
  :$$: ErrText "has potential paths with the right labels" :<>: ErrShowType ns1
  :$$: ErrText "which point at types" :<>: ErrShowType vs1 :<>: ErrText "respectively"
  :$$: ErrText "but none of these match the desired type" :<>: ErrShowType v

-- | see 'hLookupByLabelPath'
hLookupByLabelDredge l r = labelPathEndingWith (toProxy r) l `hLookupByLabelPath` r
  where toProxy :: r x -> Proxy x
        toProxy _ = Proxy

{- | lookup along a path

>>> let v = mkVariant1 Label (mkVariant1 Label 'r') :: Variant '[Tagged "x" (Variant '[Tagged "y" Char])]
>>> let r = hBuild (hBuild 'r') :: Record '[Tagged "x" (Record '[Tagged "y" Char])]
>>> let p = Label :: Label [Label "x", Label "y"]
>>> let lx = Label :: Label "y"

>>> hLookupByLabelPath p v
Just 'r'

>>> hLookupByLabelPath p r
'r'

>>> hLookupByLabelDredge lx v
Just 'r'

>>> hLookupByLabelDredge lx r
'r'

-}
hLookupByLabelPath :: HasFieldPath False ls r v => Label ls -> r -> v
hLookupByLabelPath labels r = hLookupByLabelPath1 hFalse labels r

{- |

> hLens'Path labc == hLens' la . hLens' lb . hLens' lc
>  where
>       la :: Label "a"
>       lb :: Label "b"
>       lc :: Label "c"
>       labc :: Label '["a", "b", "c"]

-}
class LabelablePath (xs :: [*]) apb spt | spt xs -> apb where
    hLens'Path :: Label xs -> apb -> spt

instance (Labelable x r s t a b,
          j ~ (a `p` f b),
          k ~ (r s `p` f (r t)),
          ty ~ LabelableTy r,
          LabeledOpticP ty p,
          LabeledOpticF ty f,
          LabeledOpticTo ty x (->),
          LabelablePath xs i j) => LabelablePath (Label x ': xs) i k where
    hLens'Path _ = (hLens' (Label :: Label x) :: j -> k) . hLens'Path (Label :: Label xs)

instance (x ~ x') => LabelablePath '[] x x' where
    hLens'Path _ = id

class HasFieldPath (needJust :: Bool) (ls :: [*]) r v | needJust ls r -> v where
    -- | use 'hLookupByLabelPath' instead
    hLookupByLabelPath1 :: Proxy needJust -> Label ls -> r -> v

instance HasFieldPath False '[] v v where
    hLookupByLabelPath1 _ _ = id

instance HasFieldPath True '[] v (Maybe v) where
    hLookupByLabelPath1 _ _ = Just

instance (HasField l (Record r) u, HasFieldPath needJust ls u v)
    => HasFieldPath needJust (Label l ': ls) (Record r) v where
     hLookupByLabelPath1 needJust _ = hLookupByLabelPath1 needJust (Label :: Label ls)
                                . hLookupByLabel (Label :: Label l)

instance (HasField l (Variant r) (Maybe u), HasFieldPath True ls u (Maybe v))
    => HasFieldPath needJust (Label l ': ls) (Variant r) (Maybe v) where
     hLookupByLabelPath1 _ _ v = hLookupByLabelPath1 hTrue (Label :: Label ls) =<< hLookupByLabel (Label :: Label l) v





{- | @(FieldTree r ns, FieldTreeVal r vs)@

defines ns and vs such that looking up path (ns !! i) in r gives the type
(vs !! i). This is almost @HasFieldPath False (ns !! i) (vs !! i)@, except
there is no additional Maybe when a Variant is encountered along the path
(and we don't have a type level @!!@)
-}
class FieldTreeVal (r :: *) (v :: [*]) | r -> v

class MapFieldTreeVal (r :: *) (ns :: Maybe [*]) (vs :: [*]) | r ns -> vs

instance (TryCollectionList r ns, MapFieldTreeVal r ns v) => FieldTreeVal r v

instance MapFieldTreeVal r Nothing '[]

instance ( MapFieldTreeVal r (Just xs) out2,
           FieldTreeVal v out1,
           (v ': HAppendListR out1 out2) ~ out)
  => MapFieldTreeVal r (Just (Tagged n v ': xs))  out

instance MapFieldTreeVal r (Just '[]) '[]

{- | list all paths through nested records or variants.
An example instance would be

> FieldTree r v

where

> v ~ [[ Label "x",  Label Dat ], '[Label "y"], '[Label "x"] ]
> r ~ Record [ Tagged "x" x, Tagged "y" String ]
>
> x ~ Variant '[ Tagged Dat Char ]

-}
class FieldTree (r :: *) (v :: [[*]]) | r -> v

-- | the only instance
instance (TryCollectionList r ns, MapFieldTree ns vs) => FieldTree r vs


#if (__GLASGOW_HASKELL__ >= 800)
-- possibly https://ghc.haskell.org/trac/ghc/ticket/13284
-- dredge' x = (isSimple . dredge) x
--     • Overlapping instances for TryCollectionList r0 ns0
--         arising from a use of ‘dredge’
--       Matching instances:
--         instance [overlappable] nothing ~ 'Nothing =>
--                                 TryCollectionList x nothing
--           -- Defined at /home/aavogt/wip/HList/HList/Data/HList/Dredge.hs:340:31
--         ...plus four instances involving out-of-scope types
--         (use -fprint-potential-instances to see them all)
--       (The choice depends on the instantiation of ‘r0, ns0’
--        To pick the first instance above, use IncoherentInstances
--        when compiling the other instance declarations)
--
-- attempt to resolve that with a closed type family

type family TryCollectionListTF (r :: *) :: Maybe [*] where
  TryCollectionListTF (Record r) = Just r
  TryCollectionListTF (Variant r) = Just r
  TryCollectionListTF (TIC r) = Just r
  TryCollectionListTF (TIP r) = Just r
  TryCollectionListTF nothing = Nothing

type TryCollectionList r v = (v ~ TryCollectionListTF r)

#else
-- | try to extract the list applied to the Record or Variant
class TryCollectionList (r :: *) (v :: Maybe [*]) | r -> v

instance {-# OVERLAPPABLE #-} (nothing ~ Nothing) => TryCollectionList x nothing
instance {-# OVERLAPPING  #-} TryCollectionList (Record  r) (Just r)
instance {-# OVERLAPPING  #-} TryCollectionList (Variant r) (Just r)
instance {-# OVERLAPPING  #-} TryCollectionList (TIC r) (Just r)
instance {-# OVERLAPPING  #-} TryCollectionList (TIP r) (Just r)
#endif

class MapFieldTree (ns :: Maybe [*]) (vs :: [[*]]) | ns -> vs

instance MapFieldTree Nothing '[]

-- | recursive case
instance (
    MapFieldTree (Just xs) vs3,
    FieldTree v vs1,
    MapCons (Label n) ('[] ': vs1) vs2,
    HAppendListR vs2 vs3 ~ vs)
    => MapFieldTree (Just (Tagged n v ': xs)) vs

instance MapFieldTree (Just '[]) '[]

-- | MapCons x xs xxs is like  xxs = map (x : ) xs
class MapCons (x :: k) (xs :: [[k]]) (xxs :: [[k]]) | x xs -> xxs
instance MapCons x '[] '[]
instance MapCons x b r => MapCons x (a ': b) ( (x ':  a) ': r)


