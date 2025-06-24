{- | Description: records where elements are stored in unboxed arrays

The public interface is exported from <Data-HList-CommonMain.html#t:RecordU RecordU>

-}
module Data.HList.RecordU where

import Data.Array.Unboxed
import Data.HList.FakePrelude
import Data.HList.Record
import Data.HList.HList

import Data.HList.HArray
import LensDefs

import Data.HList.Labelable

import Unsafe.Coerce

-- * Type definitions
-- ** RecordUS

{- | 'RecordUS' is stored as a 'HList' of 'RecordU' 
to allow the 'RecordUS' to contain elements of different
types, so long all of the types can be put into an unboxed
array ('UArray').

It is advantageous (at least space-wise) to sort the record to keep
elements with the same types elements adjacent. See 'SortForRecordUS'
for more details.  -}
newtype RecordUS (x :: [*]) =
    RecordUS Any -- ^ Any here is the @HList u@
                 -- given @'RecordUSCxt' x u@

-- | connect the unpacked @x@ representation with the
-- corresponding list of RecordU @u@ representation.
class RecordUSCxt (x :: [*]) (u :: [*]) | x -> u, u -> x where
  {- | @O(1)@ should be possible to implement this without
  unsafeCoerce, but we want to hide the @u@ parameter _and_
  keep the RecordUSCxt as a class (instead of a type
  family) because of 'HEq'. In some cases it is possible
  to have instances that do not actually respect the functional
  dependency, but this should be safe if the check is not
  disabled (by using @-XDysfunctionalDependencies@
  <https://phabricator.haskell.org/D69>, or ghc-7.6) -}
  recordUSToHList :: RecordUS x -> HList u
  recordUSToHList (RecordUS x) = unsafeCoerce x

  -- | @O(1)@ should be possible to implement this without
  -- unsafeCoerce
  hListToRecordUS :: HList u -> RecordUS x
  hListToRecordUS x = RecordUS (unsafeCoerce x)

-- | the only instance
instance (HGroupBy EqTagValue x g, HMapUnboxF g u) => RecordUSCxt x u

data EqTagValue
instance HEqByFn EqTagValue
instance (txv ~ Tagged x v,
          tyw ~ Tagged y w,
          HEq v w b) => HEqBy EqTagValue txv tyw b

-- | proof that @'hMap' 'UnboxF' :: r xs -> r us@ can determine
-- @xs@ from @us@ and @us@ from @xs@
class HMapUnboxF (xs :: [*]) (us :: [*]) | xs -> us, us -> xs
instance HMapUnboxF '[] '[]
instance HMapUnboxF xs us => HMapUnboxF (HList x ': xs) (RecordU x ': us)


instance (RecordUSCxt x u, Show (HList u)) => Show (RecordUS x) where
    showsPrec n r = ("RecordUS " ++) . showsPrec n (recordUSToHList r)

-- ** RecordU

{- | A type which behaves similarly to 'Record', except
all elements must fit in the same 'UArray'. A consequence of
this is that @RecordU@ has the following properties:

* it is strict in the element types

* it cannot do type-changing updates of 'RecordU', except if
  the function applies to all elements

* it probably is slower to update the very first elements
  of the 'RecordU'

The benefit is that lookups should be faster and records
should take up less space. However benchmarks done with
a slow 'HNat2Integral' do not suggest that RecordU is
faster than Record.
-}
newtype RecordU l = RecordU (UArray Int (GetElemTy l))

type family GetElemTy (x :: [*]) :: *
type instance GetElemTy (Tagged label v ': rest) = v

deriving instance (Show (UArray Int (GetElemTy l))) => Show (RecordU l)
deriving instance (Read (UArray Int (GetElemTy l))) => Read (RecordU l)
deriving instance (Eq  (UArray Int (GetElemTy l))) => Eq  (RecordU l)
deriving instance (Ord (UArray Int (GetElemTy l))) => Ord (RecordU l)

{- | Reorders a 'Record' such that the 'RecordUS' made from it takes up
less space

'Bad' has alternating Double and Int fields

>>> bad
Record{x=1.0,i=2,y=3.0,j=4}

4 arrays containing one element each are needed when this
Record is stored as a RecordUS

>>> recordToRecordUS bad
RecordUS H[RecordU (array (0,0) [(0,1.0)]),RecordU (array (0,0) [(0,2)]),RecordU (array (0,0) [(0,3.0)]),RecordU (array (0,0) [(0,4)])]

It is possible to sort the record

>>> sortForRecordUS bad
Record{x=1.0,y=3.0,i=2,j=4}

This allows the same content to be stored in
two unboxed arrays

>>> recordToRecordUS (sortForRecordUS bad)
RecordUS H[RecordU (array (0,1) [(0,1.0),(1,3.0)]),RecordU (array (0,1) [(0,2),(1,4)])]

-}
class SortForRecordUS x x' | x -> x' where
    sortForRecordUS :: Record x -> Record x'

instance SortForRecordUS '[] '[] where
    sortForRecordUS = id

instance (HPartitionEq EqTagValue x (x ': xs) xi xo,
          SortForRecordUS xo xo',
          sorted ~ HAppendListR xi xo',
          HAppendList xi xo') =>
  SortForRecordUS (x ': xs) sorted where
  sortForRecordUS (Record xs) = Record (hAppendList xi xo')
    where
      f  = Proxy :: Proxy EqTagValue
      x1 = Proxy :: Proxy x
      (xi,xo) = hPartitionEq f x1 xs
      Record xo' = sortForRecordUS (Record xo)

-------------------------------------------------------------- 
-- * Lookup

-- | works expected. See examples attached to 'bad'.
instance (HFindLabel l r n,
          HLookupByHNatUS n u (Tagged l v),
          HasField l (Record r) v,
          RecordUSCxt r u) =>
  HasField l (RecordUS r) v where
  hLookupByLabel _ u = case hLookupByHNatUS n (recordUSToHList u) of Tagged v -> v
    where n = Proxy :: Proxy n

class HLookupByHNatUS (n :: HNat) (us :: [*]) (e :: *) | n us -> e where
  hLookupByHNatUS :: Proxy n -> HList us -> e

class HLookupByHNatUS1 (r :: Either HNat HNat) (n :: HNat) (u :: [*]) (us :: [*]) (e :: *)
        | r n u us -> e where
  hLookupByHNatUS1 :: Proxy r -> Proxy n -> RecordU u -> HList us -> e

instance (r ~ HSubtract (HLength u) n,
          RecordU u ~ ru,
          HLookupByHNatUS1 r n u us e) =>
  HLookupByHNatUS n (ru ': us) e where
  hLookupByHNatUS n (HCons u us) = hLookupByHNatUS1 (Proxy :: Proxy r) n u us

instance (HNat2Integral n,
         HLookupByHNatR n u ~ le,
         le ~ Tagged l e,
         IArray UArray e,
         e ~ GetElemTy u) => HLookupByHNatUS1 (Left t) n u us le where
  hLookupByHNatUS1 _ n (RecordU u) _us = Tagged (u ! hNat2Integral n)

instance HLookupByHNatUS t us e => HLookupByHNatUS1 (Right t) n u us e where
  hLookupByHNatUS1 _ _ _ = hLookupByHNatUS (Proxy :: Proxy t)

-- | @HSubtract a b@ is @Left (a-b)@, @Right (b-a)@ or @Right HZero@
type family HSubtract (n1 :: HNat) (n2 :: HNat) :: Either HNat HNat

type instance HSubtract HZero HZero = Right HZero
type instance HSubtract (HSucc x) (HSucc y) = HSubtract x y
type instance HSubtract HZero (HSucc y) = Right (HSucc y)
type instance HSubtract (HSucc y) HZero = Left (HSucc y)




-------------------------------------------------------------- 
-- * Conversion of RecordUS

-- ** with the actual representation

-- | @Iso (HList s) (HList t) (RecordUS a) (RecordUS b)@
recordUS r = iso hListToRecordUS recordUSToHList r

{- | @Iso (HList s) (RecordUS a)@

@s@ is a HList of 'RecordU' while @a :: [*]@
is list of @Tagged label value@

-}
recordUS' r = isSimple recordUS r

-- ** with 'Record'

-- | @view unboxedS@ or @^. unboxedS@ are preferred
recordToRecordUS :: forall x g u.
   (HMapCxt HList UnboxF g u,
    HMapUnboxF g u,
    HGroupBy EqTagValue x g,
    RecordUSCxt x u)
   => Record x -> RecordUS x
recordToRecordUS (Record x) = hListToRecordUS u
  where
    u :: HList u
    u = hMap UnboxF g 

    g :: HList g
    g = hGroupBy (Proxy :: Proxy EqTagValue) x

-- | @^. from unboxedS@ is preferred
recordUSToRecord :: forall u g x.
  (HConcatFD g x,
   HMapCxt HList BoxF u g,
   HMapUnboxF g u,
   RecordUSCxt x u
  ) => RecordUS x -> Record x
recordUSToRecord rus = Record (hConcatFD g)
  where
    g :: HList g
    g = hMap BoxF (recordUSToHList rus)

-- | @Iso (Record x) (Record y) (RecordUS x) (RecordUS y)@
unboxedS r = iso recordToRecordUS recordUSToRecord r

-- | @Iso' (Record x) (RecordUS x)@
unboxedS' r = isSimple unboxedS r



-- | all elements of the list have the same type
class ElemTyEq (xs :: [*])

instance 
 (t1v ~ Tagged t1 v,
  t2v ~ Tagged t2 v,  
  ElemTyEq (tv2 ': rest)) =>
  ElemTyEq (tv1 ': tv2 ': rest)

instance t1v ~ Tagged t v => ElemTyEq (t1v ': rest)
instance ElemTyEq '[]


instance (IArray UArray v,
          v ~ GetElemTy ls,
          HFindLabel l ls n,
          HNat2Integral n)
    => HasField l (RecordU ls) v where
  hLookupByLabel _ (RecordU ls) = ls ! hNat2Integral (Proxy :: Proxy n)


instance (r ~ r',
          v ~ GetElemTy r,
          HFindLabel l r n,
          HNat2Integral n,
          IArray UArray v,
          HasField l (Record r') v)
    => HUpdateAtLabel RecordU l v r r' where
  hUpdateAtLabel _ v (RecordU r) = RecordU (r // [(hNat2Integral (Proxy :: Proxy n), v)])


{- | analogous flip '//'. Similar to '.<++.', except it is restricted
to cases where the left argument holds a subset of elements.

-}
class HUpdateMany lv rx where
    hUpdateMany :: Record lv -> rx -> rx

instance (RecordValues lv,
          HList2List (RecordValuesR lv) v,
          HFindMany (LabelsOf lv) (LabelsOf r) ixs,
          IArray UArray v,
          v ~ GetElemTy r,
          HNats2Integrals ixs) =>
  HUpdateMany lv (RecordU r) where
  hUpdateMany lv (RecordU r) = RecordU (r // (zip ixs (hList2List (recordValues lv))))
     where ixs = hNats2Integrals (Proxy :: Proxy ixs)

-- | implementation in terms of '.<++.'
instance (HLeftUnion lv x lvx,
          HRLabelSet x,
          HLabelSet (LabelsOf x),
          HRearrange (LabelsOf x) lvx x)
  => HUpdateMany lv (Record x) where
    hUpdateMany lv x = hRearrange' (lv .<++. x)

-- | behaves like @map 'HFind'@
class HFindMany (ls :: [k]) (r :: [k]) (ns :: [HNat]) | ls r  -> ns
instance (HFind l r n,
          HFindMany ls r ns) => HFindMany (l ': ls) r (n ': ns)

instance HFindMany '[] r '[]

instance (ApplyAB f (GetElemTy x) (GetElemTy y),
          IArray UArray (GetElemTy y),
          IArray UArray (GetElemTy x)) => HMapAux RecordU f x y where
    hMapAux f (RecordU x) = RecordU (amap (applyAB f) x)

-- | 'hMap' specialized to 'RecordU'
hMapRU :: HMapCxt RecordU f x y => f -> RecordU x -> RecordU y
hMapRU f = hMap f


-- | @Iso (Record x) (Record y) (RecordU x) (RecordU y)@
unboxed :: forall x y f p.
  (Profunctor p,
   Functor f,
   RecordToRecordU x,
   RecordUToRecord y)
  => RecordU x `p` f (RecordU y)
  -> Record x `p` f (Record y)
unboxed r = iso recordToRecordU recordUToRecord r

-- | @Iso' (Record x) (RecordU x)@
unboxed' x = isSimple unboxed x


class RecordToRecordU x where
    recordToRecordU :: Record x -> RecordU x

instance (
    RecordValues x,
    HList2List (RecordValuesR x) (GetElemTy x),
    HNat2Integral n,
    HLengthEq x n,
    IArray UArray (GetElemTy x)
   ) => RecordToRecordU x where
  recordToRecordU (rx@(Record x)) = RecordU $ listArray
          (0, hNat2Integral (hLength x) - 1)
          (hList2List (recordValues rx))
 
class RecordUToRecord x where
    recordUToRecord :: RecordU x -> Record x

instance (
    HMapCxt HList TaggedFn (RecordValuesR x) x,
    IArray UArray (GetElemTy x),
    HList2List (RecordValuesR x) (GetElemTy x) 
  ) => RecordUToRecord x where
  recordUToRecord (RecordU b) = case list2HList $ elems b of
          Nothing -> error "Data.HList.RecordU.recordUToRecord impossibly too few elements"
          Just y0 -> Record $ hMap TaggedFn (y0 :: HList (RecordValuesR x))



-- * definitions for doctest examples
type Bad =
         [Tagged "x" Double,
          Tagged "i" Int,
          Tagged "y" Double,
          Tagged "j" Int]

{- | HasField instances

[@RecordUS@]

>>> let r = recordToRecordUS (sortForRecordUS bad)
>>> let s = recordToRecordUS bad

>>> let x = Label :: Label "x"
>>> let y = Label :: Label "y"
>>> let i = Label :: Label "i"
>>> let j = Label :: Label "j"

>>> (r .!. x, r .!. i, r .!. y, r .!. j)
(1.0,2,3.0,4)

>>> (s .!. x, s .!. i, s .!. y, s .!. j)
(1.0,2,3.0,4)


[@RecordU@]

>>> let t = recordToRecordU bad1
>>> (t .!. x, t .!. y)
(1.0,2.0)

>>> hUpdateAtLabel x 3 t .!. x
3.0

-}
bad :: Record Bad
bad = Tagged 1 .*. Tagged 2 .*. Tagged 3 .*. Tagged 4 .*. emptyRecord

bad1 :: Record [Tagged "x" Double, Tagged "y" Double]
bad1 = Tagged 1 .*. Tagged 2 .*. emptyRecord

-- * Implementation Details

data UnboxF = UnboxF
instance (hx ~ HList x, ux ~ RecordU x,
          RecordToRecordU x) =>
  ApplyAB UnboxF hx ux where
  applyAB _ = recordToRecordU . Record

data BoxF = BoxF

instance (ux ~ RecordU x,
         hx ~ HList x,
         RecordUToRecord x) =>
  ApplyAB BoxF ux hx where
  applyAB _ ux = case recordUToRecord ux of Record hx -> hx


-- | make a @Lens' (RecordU s) a@
instance (s ~ t, a ~ b,
          IArray UArray a, a ~ GetElemTy s,
          HLensCxt x RecordU s t a b)
        => Labelable x RecordU s t a b where
            type LabelableTy RecordU = LabelableLens
            hLens' x = hLens x

{- TODO
instance Labelable x RecordUS to p f s t a b where
instance (r ~ r', HasField l (Record r) v)
      => HUpdateAtLabel RecordUS l v r r' where
  hUpdateAtLabel = error "recordus hupdateatlabel"

Benchmarks
-}
