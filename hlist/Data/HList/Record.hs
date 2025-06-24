{-# LANGUAGE CPP #-}
{- |
   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Extensible records

   The three-ish models of labels that go with this module;

   * "Data.HList.Label3"

   * "Data.HList.Label5"

   * "Data.HList.Label6"

   * "Data.HList.Labelable"


   These used to work:

   * "Data.HList.Label1"

   * "Data.HList.Label2"

   * "Data.HList.Label4"

-}

module Data.HList.Record
(
    -- ** labels used for doctests
    -- $setup

    -- * Records

    -- ** Labels
    -- $labels
    module Data.Tagged,
    (.=.),

    -- ** Record
    Record(..),
    mkRecord,
    emptyRecord,
    hEndR,
    hEndP,

    hListRecord, hListRecord',

    -- *** Getting Labels
    LabelsOf,
    labelsOf,
    asLabelsOf,

    -- *** Getting Values
    RecordValues(..),
    recordValues,
    hMapTaggedFn,

    unlabeled0,

    Unlabeled,
    unlabeled,
    Unlabeled',
    unlabeled',

    -- * Operations
    -- ** Show
    -- | A corresponding 'Show' instance exists as
    --
    -- > show x = "Record {" ++ showComponents "" x ++ "}"
    ShowComponents(..),
    ShowLabel(..),

    -- ** Extension
    -- | 'hExtend', 'hAppend'
    (.*.),

    -- ** Delete
    -- | 'hDeleteAtLabel' @label record@
    (.-.),
    HDeleteLabels(..),

    -- ** Lookup/update
    -- $lens
    HLens(hLens),

    -- ** Lookup
    HasField(..),
    HasFieldM(..),
    (.!.),

    -- ** Update
    (.@.),
    HUpdateAtLabel(hUpdateAtLabel),
    -- *** type-preserving versions
    -- | Note: these restrict the resulting record type to be the same as in
    -- input record type, which can help reduce the number of type annotations
    -- needed
    (.<.),
    HTPupdateAtLabel,
    hTPupdateAtLabel,

    -- ** Rename Label
    hRenameLabel,

    -- ** Projection

    Labels,

    -- $projection
    hProjectByLabels,
    hProjectByLabels',
    hProjectByLabels2,

    -- *** a lens for projection
    -- | see "Data.HList.Labelable".'Projected'

    -- ** Unions
    -- *** Left
    HLeftUnion(hLeftUnion),
    (.<++.),

    -- *** Symmetric
    -- $symmetricUnion
    UnionSymRec(unionSR),


    -- ** Reorder Labels
    hRearrange,
    hRearrange',

    -- *** isos using hRearrange
    Rearranged(rearranged), rearranged',


    -- ** Apply a function to all values
    hMapR, HMapR(..),

    -- ** cast labels
    Relabeled(relabeled),
    relabeled',

    -- * Hints for type errors
    DuplicatedLabel,
    ExtraField,
    FieldNotFound,

    -- * Unclassified

    -- | Probably internals, that may not be useful
#if __GLASGOW_HASKELL__ != 706
    zipTagged,
#endif
    HasField'(..),
    DemoteMaybe,
    HasFieldM1(..),
    H2ProjectByLabels(h2projectByLabels),
    H2ProjectByLabels'(h2projectByLabels'),
    HLabelSet,
    HLabelSet',
    HRLabelSet,
    HAllTaggedLV,
    HRearrange(hRearrange2),
    HRearrange3(hRearrange3),
    HRearrange4(hRearrange4),
    UnionSymRec'(..),
    HFindLabel,
    labelLVPair,
    newLVPair,
    UnLabel,
    HMemberLabel,
    TaggedFn(..),
    ReadComponent,
    HMapTaggedFn,
    HLensCxt,

    -- ** zip
    -- | use the more general 'HZip' class instead
    HZipRecord(..),
    -- *** alternative implementation
    hZipRecord2, hUnzipRecord2
) where


import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HList

import Data.HList.Label3 (MapLabel)

import Data.Tagged
import Control.Monad

import Text.ParserCombinators.ReadP

import LensDefs

import Data.Array (Ix)
#if __GLASGOW_HASKELL__ <= 906
import Data.Semigroup (Semigroup)
#endif
-- imports for doctest/examples
import Data.HList.Label6 ()
import Data.HList.TypeEqO ()

{- $setup

>>> let x = Label :: Label "x"
>>> let y = Label :: Label "y"
>>> let z = Label :: Label "z"

-}

-- --------------------------------------------------------------------------

-- $labels Record types as label-value pairs, where label is purely phantom.
-- Thus the run-time representation of a field is the same as that of
-- its value, and the record, at run-time, is indistinguishable from
-- the HList of field values. At run-time, all information about the
-- labels is erased.
--
-- The type from "Data.Tagged" is used.

-- | Label accessor
labelLVPair :: Tagged l v -> Label l
labelLVPair _ = Label

newLVPair :: Label l -> v -> Tagged l v
newLVPair _ = Tagged



infixr 4 .=.
{-|

  Create a value with the given label. Analagous to a data
  constructor such as 'Just', 'Left', or 'Right'. Higher fixity
  than record-modification operations like ('.*.'), ('.-.'), etc. to
  support expression like the below w/o parentheses:

  >>> x .=. "v1" .*. y .=. '2' .*. emptyRecord
  Record{x="v1",y='2'}

-}
(.=.) :: Label l -> v -> Tagged l v
l .=. v = newLVPair l v


newtype Record (r :: [*]) = Record (HList r)

deriving instance Semigroup (HList r) => Semigroup (Record r)
deriving instance Monoid (HList r) => Monoid (Record r)
deriving instance (Eq (HList r)) => Eq (Record r)
deriving instance (Ord (HList r)) => Ord (Record r)
deriving instance (Ix (HList r)) => Ix (Record r)
deriving instance (Bounded (HList r)) => Bounded (Record r)


-- | Build a record
mkRecord :: HRLabelSet r => HList r -> Record r
mkRecord = Record

-- | @HRLabelSet t => Iso (HList s) (HList t) (Record s) (Record t)@
hListRecord x = isoNewtype mkRecord (\(Record r) -> r) x

-- | @Iso' (HList s) (Record s)@
hListRecord' x = isSimple hListRecord x

-- | Build an empty record
emptyRecord :: Record '[]
emptyRecord = mkRecord HNil

-- | @Iso (Record s) (Record t) (HList a) (HList b)@
--
-- @view unlabeled == 'recordValues'@
unlabeled0 x = sameLabels (iso recordValues hMapTaggedFn x)


unlabeled :: (Unlabeled x y, Profunctor p, Functor f) =>
    (HList (RecordValuesR x) `p` f (HList (RecordValuesR y))) ->
    (Record x `p` f (Record y))
unlabeled x = sameLength (unlabeled0 (sameLength x))

type Unlabeled x y =
      (HMapCxt HList TaggedFn (RecordValuesR y) y,
       RecordValues x, RecordValues y,
       SameLength (RecordValuesR x) (RecordValuesR y),
       SameLength x y, SameLabels x y,
       HAllTaggedLV x, HAllTaggedLV y)
type Unlabeled' x = Unlabeled x x



-- | @Unlabeled' x => Iso' (Record x) (HList (RecordValuesR x))@
unlabeled' :: (Unlabeled' x, Profunctor p, Functor f) =>
    (HList (RecordValuesR x) `p` f (HList (RecordValuesR x))) ->
    (Record x `p` f (Record x))
unlabeled' = unlabeled

{- | @Iso (Record s) (Record t) (Record a) (Record b)@, such that
@relabeled = unlabeled . from unlabeled@

in other words, pretend a record has different labels, but the same values.

-}
class Relabeled r where
  relabeled :: forall p f s t a b.
      (HMapTaggedFn (RecordValuesR s) a,
       HMapTaggedFn (RecordValuesR b) t,
       SameLengths '[s,a,t,b],
       RecordValuesR t ~ RecordValuesR b,
       RecordValuesR s ~ RecordValuesR a,
       RecordValues b, RecordValues s,
       Profunctor p,
       Functor f
       ) => r a `p` f (r b) -> r s `p` f (r t)

instance Relabeled Record where
  relabeled = iso
    (\ s -> hMapTaggedFn (recordValues s))
    (\ b -> hMapTaggedFn (recordValues b))
    -- isoNewtype should be safe here, but there are no guarantees
    -- http://stackoverflow.com/questions/24222552

-- | @Iso' (Record s) (Record a)@
--
-- such that @RecordValuesR s ~ RecordValuesR a@
relabeled' x = isSimple relabeled x

data TaggedFn = TaggedFn
instance (tx ~ Tagged t x) => ApplyAB TaggedFn x tx where
    applyAB _ = Tagged

type HMapTaggedFn l r =
    (HMapCxt HList TaggedFn l r,
     RecordValuesR r ~ l,
     RecordValues r)

-- | \"inverse\" to 'recordValues'
hMapTaggedFn :: HMapTaggedFn a b => HList a -> Record b
hMapTaggedFn = Record . hMap TaggedFn

-- | Property of a proper label set for a record: no duplication of labels,
-- and every element of the list is @Tagged label value@

data DuplicatedLabel l

class (HLabelSet (LabelsOf ps), HAllTaggedLV ps) => HRLabelSet (ps :: [*])
instance (HLabelSet (LabelsOf ps), HAllTaggedLV ps) => HRLabelSet (ps :: [*])




{- | Relation between HLabelSet and HRLabelSet

> instance HLabelSet (LabelsOf ps) => HRLabelSet ps

see also 'HSet'
-}

class HLabelSet ls
instance HLabelSet '[]
instance HLabelSet '[x]
instance ( HEqK l1 l2 leq
         , HLabelSet' l1 l2 leq r
         ) => HLabelSet (l1 ': l2 ': r)

class HLabelSet' l1 l2 (leq::Bool) r
instance ( HLabelSet (l2 ': r)
         , HLabelSet (l1 ': r)
         ) => HLabelSet' l1 l2 False r
instance ( Fail (DuplicatedLabel l1) ) => HLabelSet' l1 l2 True r

-- | Construct the (phantom) list of labels of a record,
-- or list of Label.
type family LabelsOf (ls :: [*]) :: [*]
type instance LabelsOf '[] = '[]
type instance LabelsOf (Label l ': r)  = Label l ': LabelsOf r
type instance LabelsOf (Tagged l v ': r) = Label l ': LabelsOf r

labelsOf :: hlistOrRecord l -> Proxy (LabelsOf l)
labelsOf _ = Proxy



-- | remove the Label type constructor. The @proxy@ argument is
-- supplied to make it easier to fix the kind variable @k@.
type family UnLabel (proxy :: k) (ls :: [*]) :: [k]
type instance UnLabel proxy (Label x ': xs) = x ': UnLabel proxy xs
type instance UnLabel proxy '[] = '[]

-- | A version of 'HFind' where the @ls@ type variable is a list of
-- 'Tagged' or 'Label'. This is a bit indirect, and ideally LabelsOf
-- could have kind [*] -> [k].
type HFindLabel (l :: k) (ls :: [*]) (n :: HNat) = HFind l (UnLabel l (LabelsOf ls)) n

-- | Construct the HList of values of the record.
class SameLength r (RecordValuesR r)
      => RecordValues (r :: [*]) where
  type RecordValuesR r :: [*]
  recordValues' :: HList r -> HList (RecordValuesR r)

instance RecordValues '[] where
  type RecordValuesR '[] = '[]
  recordValues' _ = HNil
instance (SameLength' r (RecordValuesR r),
          SameLength' (RecordValuesR r) r, RecordValues r) => RecordValues (Tagged l v ': r) where
   type RecordValuesR (Tagged l v ': r) = v ': RecordValuesR r
   recordValues' (HCons (Tagged v) r) = HCons v (recordValues' r)

recordValues :: RecordValues r => Record r -> HList (RecordValuesR r)
recordValues (Record r) = recordValues' r

{- shorter, but worse in terms needing annotations to allow ambiguous types
- but better in terms of inference
recordValues :: RecordValues r rv => Record r -> HList rv
recordValues (Record r) = hMap HUntag r

type RecordValues r rv = HMapCxt HUntag r rv
-}

-- --------------------------------------------------------------------------

-- 'Show' instance to appeal to normal records

instance ShowComponents r => Show (Record r) where
  show (Record r) =  "Record{"
                  ++ showComponents "" r
                  ++ "}"

class ShowComponents l where
  showComponents :: String -> HList l -> String

instance ShowComponents '[] where
  showComponents _ _ = ""

instance ( ShowLabel l
         , Show v
         , ShowComponents r
         )
      =>   ShowComponents (Tagged l v ': r) where
  showComponents comma (HCons f@(Tagged v) r)
     =  comma
     ++ showLabel ((labelLVPair f) :: Label l)
     ++ "="
     ++ show v
     ++ showComponents "," r


-- --------------------------------------------------------------------------

-- 'Read' instance to appeal to normal records


data ReadComponent = ReadComponent Bool -- ^ include comma?

instance (Read v, ShowLabel l,
          x ~ Tagged l v,
          ReadP x ~ y) =>
  ApplyAB ReadComponent (Proxy x) y where
    applyAB (ReadComponent comma) _ = do
      when comma (() <$ string ",")
      _ <- string (showLabel (Label :: Label l))
      _ <- string "="
      v <- readS_to_P reads
      return (Tagged v)


instance (HMapCxt HList ReadComponent (AddProxy rs) bs,
          ApplyAB ReadComponent (Proxy r) readP_r,
          HProxies rs,
          HSequence ReadP (readP_r ': bs) (r ': rs),
          readP_r ~ ReadP (Tagged l v),

          -- ghc-8.0.2 needs these. The above constraints
          -- should imply them
          r ~ Tagged l v,
          ShowLabel l,
          Read v,
          HSequence ReadP bs rs
          ) => Read (Record (r ': rs)) where
    readsPrec _ = readP_to_S $ do
        _ <- string "Record{"
        content <- hSequence parsers
        _ <- string "}"
        return (Record content)

      where
        rs :: HList (AddProxy rs)
        rs = hProxies

        readP_r :: readP_r
        readP_r = applyAB
                      (ReadComponent False)
                      (Proxy :: Proxy r)

        parsers = readP_r `HCons` (hMap (ReadComponent True) rs :: HList bs)





-- --------------------------------------------------------------------------

-- Extension

instance HRLabelSet (t ': r)
    => HExtend t (Record r) where
  type HExtendR t (Record r) = Record (t ': r)
  f .*. (Record r) = mkRecord (HCons f r)


-- * For records

{-|

  [@(.*.)@]
           Add a field to a record. Analagous to (++) for
           lists.

  > record .*. field1
  >        .*. field2

-}

-- --------------------------------------------------------------------------

-- Concatenation

instance (HRLabelSet (HAppendListR r1 r2), HAppend (HList r1) (HList r2))
    => HAppend (Record r1) (Record r2) where
  hAppend (Record r) (Record r') = mkRecord (hAppend r r')

type instance HAppendR (Record r1) (Record r2) = Record (HAppendListR r1 r2)
-- --------------------------------------------------------------------------

-- Lookup
--
-- |
-- This is a baseline implementation.
-- We use a helper class, HasField, to abstract from the implementation.

-- | Because 'hLookupByLabel' is so frequent and important, we implement
-- it separately, more efficiently. The algorithm is familiar assq, only
-- the comparison operation is done at compile-time
class HasField (l::k) r v | l r -> v where
    hLookupByLabel:: Label l -> r -> v

{- alternative "straightforward" implementation
instance ( LabelsOf r ~ ls
         , HFind l ls n
         , HLookupByHNat n r
         , HLookupByHNatR n r ~ LVPair l v
         ) => HasField l (Record r) v
  where
    hLookupByLabel l (Record r) = v
      where
        (LVPair v) = hLookupByHNat (proxy :: Proxy n) r
-}

{- | a version of 'HasField' / 'hLookupByLabel' / '.!.' that
returns a default value when the label is not in the record:

>>> let r = x .=. "the x value" .*. emptyRecord

>>> hLookupByLabelM y r ()
()

>>> hLookupByLabelM x r ()
"the x value"



-}
class HasFieldM (l :: k) r (v :: Maybe *) | l r -> v where
    hLookupByLabelM :: Label l
          -> r -- ^ Record (or Variant,TIP,TIC)
          -> t -- ^ default value
          -> DemoteMaybe t v

type family DemoteMaybe (d :: *) (v :: Maybe *) :: *
type instance DemoteMaybe d (Just a) = a
type instance DemoteMaybe d Nothing = d

class HasFieldM1 (b :: Maybe [*]) (l :: k) r v | b l r -> v where
    hLookupByLabelM1 :: Proxy b -> Label l -> r -> t -> DemoteMaybe t v

instance (HMemberM (Label l) (LabelsOf xs) b,
          HasFieldM1 b l (r xs) v)  => HasFieldM l (r xs) v where
    hLookupByLabelM = hLookupByLabelM1 (Proxy :: Proxy b)

instance HasFieldM1 Nothing l r Nothing where
    hLookupByLabelM1 _ _ _ t = t

instance HasField l r v => HasFieldM1 (Just b) l r (Just v) where
    hLookupByLabelM1 _ l r _t = hLookupByLabel l r



instance (HEqK l l1 b, HasField' b l (Tagged l1 v1 ': r) v)
    => HasField l (Record (Tagged l1 v1 ': r)) v where
    hLookupByLabel l (Record r) =
             hLookupByLabel' (Proxy::Proxy b) l r

-- | XXX
instance (t ~ Any, Fail (FieldNotFound l ())) => HasField l (Record '[]) t where
    hLookupByLabel _ _ = error "Data.HList.Record.HasField: Fail instances should not exist"


class HasField' (b::Bool) (l :: k) (r::[*]) v | b l r -> v where
    hLookupByLabel':: Proxy b -> Label l -> HList r -> v

instance HasField' True l (Tagged l v ': r) v where
    hLookupByLabel' _ _ (HCons (Tagged v) _) = v
instance HasField l (Record r) v => HasField' False l (fld ': r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l (Record r)



infixr 9 .!.
{- |
  Lookup a value in a record by its label. Analagous to (!!), the
  list indexing operation. Highest fixity, like ('!!').

  >>> :{
  let record1 = x .=. 3 .*.
                y .=. 'y' .*.
                emptyRecord
  :}


  >>> record1 .!. x
  3

  >>> record1 .!. y
  'y'


  >>> :{
  let r2 = y .=. record1 .!. x .*.
           z .=. record1 .!. y .*.
           emptyRecord
  :}

  >>> r2
  Record{y=3,z='y'}

  Note that labels made following "Data.HList.Labelable" allow
  using "Control.Lens.^." instead.
-}


(.!.) :: (HasField l r v) => r -> Label l -> v
r .!. l =  hLookupByLabel l r

-- --------------------------------------------------------------------------

-- Delete

instance (H2ProjectByLabels '[Label l] v t1 v')
      => HDeleteAtLabel Record l v v' where
  hDeleteAtLabel _ (Record r) =
    Record $ snd $ h2projectByLabels (Proxy::Proxy '[Label l]) r

infixl 2 .-.
{-|
  Remove a field from a record. At the same
  level as other record modification options ('.*.'). Analagous
  to (@\\\\@) in lists.

  > record1 .-. label1

  > label1 .=. value1 .*.
  > label2 .=. value2 .-.
  > label2 .*.
  > emptyRecord

  > label1 .=. value1 .-.
  > label1 .*.
  > label2 .=. value2 .*.
  > emptyRecord

  > record1 .*. label1 .=. record2 .!. label1
  >         .*. label2 .=. record2 .!. label2
  >         .-. label1

-}
(.-.) :: (HDeleteAtLabel r l xs xs') =>
    r xs -> Label l -> r xs'
r .-. l =  hDeleteAtLabel l r


-- --------------------------------------------------------------------------

-- Update

-- | 'hUpdateAtLabel' @label value record@

class
    HUpdateAtLabel record (l :: k) (v :: *) (r :: [*]) (r' :: [*])
          | l v r -> r', l r' -> v where
    hUpdateAtLabel :: SameLength r r' => Label l -> v -> record r -> record r'

instance (HUpdateAtLabel2 l v r r',
        HasField l (Record r') v) =>
        HUpdateAtLabel Record l v r r' where
    hUpdateAtLabel = hUpdateAtLabel2

{- alternative impl which reports a Fail constraint that is too long (the
one from HUpdateAtHNat) on ghc 7.10 RC1

instance (HasField l (Record r') v,
          HFindLabel l r n,
          HUpdateAtHNat n (Tagged l v) r,
          HUpdateAtHNatR n (Tagged l v) r ~ r',
          SameLength r r') =>
  HUpdateAtLabel Record l v r r' where
  hUpdateAtLabel l v (Record r) =
    Record (hUpdateAtHNat (Proxy::Proxy n) (newLVPair l v) r)
-}

class HUpdateAtLabel2 (l :: k) (v :: *) (r :: [*]) (r' :: [*])
        | l r v -> r' where
    hUpdateAtLabel2 :: Label l -> v -> Record r -> Record r'

class HUpdateAtLabel1 (b :: Bool) (l :: k) (v :: *) (r :: [*]) (r' :: [*])
        | b l v r -> r' where
    hUpdateAtLabel1 :: Proxy b -> Label l -> v -> Record r -> Record r'

instance HUpdateAtLabel1 True l v (Tagged l e ': xs) (Tagged l v ': xs) where
    hUpdateAtLabel1 _b _l v (Record (e `HCons` xs)) = Record (e{ unTagged = v } `HCons` xs)

instance HUpdateAtLabel2 l v xs xs' => HUpdateAtLabel1 False l v (x ': xs) (x ': xs') where
    hUpdateAtLabel1 _b l v (Record (x `HCons` xs)) = case hUpdateAtLabel2 l v (Record xs) of
        Record xs' -> Record (x `HCons` xs')

instance (HEqK l l' b, HUpdateAtLabel1 b l v (Tagged l' e ': xs) xs')
    => HUpdateAtLabel2 l v (Tagged l' e ': xs) xs' where
    hUpdateAtLabel2 = hUpdateAtLabel1 (Proxy :: Proxy b)

-- | XXX
instance Fail (FieldNotFound l ()) => HUpdateAtLabel2 l v '[] '[] where
    hUpdateAtLabel2 _ _ r = r


infixr 2 .@.
{-|

  Update a field with a particular value.
  Same fixity as (.*.) so that extensions and updates can be chained.
  There is no real list analogue, since there is no Prelude defined
  update.

  > label1 .=. value1 .@. record1

-}
f@(Tagged v) .@. r  =  hUpdateAtLabel (labelLVPair f) v r


-- --------------------------------------------------------------------------
-- Projection

-- $projection
-- It is also an important operation: the basis of many
-- deconstructors -- so we try to implement it efficiently.


-- | @hProjectByLabels ls r@ returns @r@ with only the labels in @ls@ remaining
hProjectByLabels :: (HRLabelSet a, H2ProjectByLabels ls t a b) =>
        proxy ls -> Record t -> Record a
hProjectByLabels ls (Record r) = mkRecord (fst $ h2projectByLabels ls r)

-- | See 'H2ProjectByLabels'
hProjectByLabels2 ::
    (H2ProjectByLabels ls t t1 t2, HRLabelSet t1, HRLabelSet t2) =>
    Proxy ls -> Record t -> (Record t1, Record t2)
hProjectByLabels2 ls (Record r) = (mkRecord rin, mkRecord rout)
   where (rin,rout) = h2projectByLabels ls r

-- need to rearrange because the ordering in the result is determined by
-- the ordering in the original record, not the ordering in the proxy. In
-- other words,
--
-- > hProjectByLabels (Proxy :: Proxy ["x","y"]) r == hProjectByLabels (Proxy :: Proxy ["y","x"]) r
hProjectByLabels' r =
    let r' = hRearrange' (hProjectByLabels (labelsOf r') r)
    in r'



{- | A helper to make the Proxy needed by hProjectByLabels,
and similar functions which accept a list of kind [*].

For example:

@(rin,rout) = 'hProjectByLabels2' (Proxy :: Labels ["x","y"]) r@

behaves like

> rin = r .!. (Label :: Label "x") .*.
>       r .!. (Label :: Label "y") .*.
>       emptyRecord
>
> rout = r .-. (Label :: Label "x") .-. (Label :: Label "y")

-}
type family Labels (xs :: [k]) :: *
type instance Labels xs = Proxy (Labels1 xs)

type family Labels1 (xs :: [k]) :: [*]
type instance Labels1 '[] = '[]
type instance Labels1 (x ': xs) = Label x ': Labels1 xs

-- | /Invariant/:
--
--  > r === rin `disjoint-union` rout
--  > labels rin === ls
--  >     where (rin,rout) = hProjectByLabels ls r
class H2ProjectByLabels (ls::[*]) r rin rout | ls r -> rin rout where
    h2projectByLabels :: proxy ls -> HList r -> (HList rin,HList rout)

instance H2ProjectByLabels '[] r '[] r where
    h2projectByLabels _ r = (HNil,r)

instance H2ProjectByLabels (l ': ls) '[] '[] '[] where
    h2projectByLabels _ _ = (HNil,HNil)

instance (HMemberM (Label l1) ((l :: *) ': ls) (b :: Maybe [*]),
          H2ProjectByLabels' b (l ': ls) (Tagged l1 v1 ': r1) rin rout)
    => H2ProjectByLabels (l ': ls) (Tagged l1 v1 ': r1) rin rout where
    h2projectByLabels = h2projectByLabels' (Proxy::Proxy b)

class H2ProjectByLabels' (b::Maybe [*]) (ls::[*]) r rin rout
                         | b ls r -> rin rout where
    h2projectByLabels' :: Proxy b -> proxy ls ->
                                     HList r -> (HList rin,HList rout)

instance H2ProjectByLabels ls1 r rin rout =>
    H2ProjectByLabels' ('Just ls1) ls (f ': r) (f ': rin) rout where
    h2projectByLabels' _ _ (HCons x r) = (HCons x rin, rout)
        where (rin,rout) = h2projectByLabels (Proxy::Proxy ls1) r

-- | if ls above has labels not in the record,
-- we get labels (rin `isSubsetOf` ls).
instance H2ProjectByLabels ls r rin rout =>
    H2ProjectByLabels' 'Nothing ls (f ': r) rin (f ': rout) where
    h2projectByLabels' _ ls (HCons x r) = (rin, HCons x rout)
        where (rin,rout) = h2projectByLabels ls r

-- --------------------------------------------------------------------------
{- | Rename the label of record

>>> hRenameLabel x y (x .=. () .*. emptyRecord)
Record{y=()}

-}
hRenameLabel l l' r = r''
 where
  v   = hLookupByLabel l r
  r'  = hDeleteAtLabel l r
  r'' = newLVPair l' v .*. r'


-- --------------------------------------------------------------------------

type HTPupdateAtLabel record l v r = (HUpdateAtLabel record l v r r, SameLength' r r)

-- | A variation on 'hUpdateAtLabel': type-preserving update.
hTPupdateAtLabel :: HTPupdateAtLabel record l v r => Label l -> v -> record r -> record r
hTPupdateAtLabel l v r = hUpdateAtLabel l v r

{- ^

We could also say:

> hTPupdateAtLabel l v r = hUpdateAtLabel l v r `asTypeOf` r

Then we were taking a dependency on Haskell's type equivalence.
This would also constrain the actual implementation of hUpdateAtLabel.

-}

infixr 2 .<.
{-|
  The same as '.@.', except type preserving. It has the same fixity as (.\@.).

-}
f@(Tagged v) .<. r = hTPupdateAtLabel (labelLVPair f) v r

-- --------------------------------------------------------------------------
-- | Subtyping for records

instance H2ProjectByLabels (LabelsOf r2) r1 r2 rout
    => SubType (Record r1) (Record r2)


type HMemberLabel l r b = HMember l (UnLabel l (LabelsOf r)) b

-- --------------------------------------------------------------------------

-- Left Union

class HDeleteLabels ks r r' | ks r -> r'
 where hDeleteLabels :: proxy (ks :: [*]) -- ^ as provided by labelsOf
          -> Record r -> Record r'

instance (HMember (Label l) ks b,
          HCond b (Record r2) (Record (Tagged l v ': r2)) (Record r3),
          HDeleteLabels ks r1 r2) =>
    HDeleteLabels ks (Tagged l v ': r1) r3 where
      hDeleteLabels ks (Record (HCons lv r1)) =
          case hDeleteLabels ks (Record r1) of
             Record r2 -> hCond (Proxy :: Proxy b)
                  (Record r2)
                  (Record (HCons lv r2))
instance HDeleteLabels ks '[] '[] where
    hDeleteLabels _ _ = emptyRecord


class  HLeftUnion r r' r'' | r r' -> r''
 where hLeftUnion :: Record r -> Record r' -> Record r''

instance (HDeleteLabels (LabelsOf l) r r',
         HAppend (Record l) (Record r'),
         HAppendR (Record l) (Record r') ~ (Record lr)) => HLeftUnion l r lr
 where  hLeftUnion l r = l `hAppend` hDeleteLabels (labelsOf l) r


infixl 1 .<++.
{-|
  Similar to list append, so give this slightly lower fixity than
  (.*.), so we can write:

   > field1 .=. value .*. record1 .<++. record2

-}
(.<++.) ::  (HLeftUnion r r' r'') => Record r -> Record r' -> Record r''
r .<++. r' = hLeftUnion r r'


-- --------------------------------------------------------------------------
-- $symmetricUnion
-- Compute the symmetric union of two records r1 and r2 and
-- return the pair of records injected into the union (ru1, ru2).
--
-- To be more precise, we compute the symmetric union /type/ @ru@
-- of two record /types/ @r1@ and @r2@. The emphasis on types is important.
--
-- The two records (ru1,ru2) in the result of 'unionSR' have the same
-- type ru, but they are generally different values.
-- Here the simple example: suppose
--
-- >  r1 = (Label .=. True)  .*. emptyRecord
-- >  r2 = (Label .=. False) .*. emptyRecord
--
-- Then 'unionSR' r1 r2 will return (r1,r2). Both components of the result
-- are different records of the same type.
--
--
-- To project from the union ru, use 'hProjectByLabels'.
-- It is possible to project from the union obtaining a record
-- that was not used at all when creating the union.
--
-- We do assure however that if @unionSR r1 r2@ gave @(r1u,r2u)@,
-- then projecting r1u onto the type of r1 gives the /value/ identical
-- to r1. Ditto for r2.

class UnionSymRec r1 r2 ru | r1 r2 -> ru where
    unionSR :: Record r1 -> Record r2 -> (Record ru, Record ru)

instance (r1 ~ r1') => UnionSymRec r1 '[] r1' where
    unionSR r1 _ = (r1, r1)

instance ( HMemberLabel l r1 b
         , UnionSymRec' b r1 (Tagged l v) r2' ru
         )
    => UnionSymRec r1 (Tagged l v ': r2') ru
    where
    unionSR r1 (Record (HCons f r2')) =
        unionSR' (Proxy::Proxy b) r1 f (Record r2')

class UnionSymRec' (b :: Bool) r1 f2 r2' ru | b r1 f2 r2' -> ru where
    unionSR' :: Proxy b -> Record r1 -> f2 -> Record r2'  -> (Record ru, Record ru)



-- | Field f2 is already in r1, so it will be in the union of r1
-- with the rest of r2.
--
-- To inject (HCons f2 r2) in that union, we should replace the
-- field f2
instance (UnionSymRec r1 r2' ru,
          HTPupdateAtLabel Record l2 v2 ru,
          f2 ~ Tagged l2 v2)
    => UnionSymRec' True r1 f2 r2' ru where
    unionSR' _ r1 (Tagged v2) r2' =
       case unionSR r1 r2'
        of (ul,ur) -> (ul, hTPupdateAtLabel (Label :: Label l2) v2 ur)



instance (UnionSymRec r1 r2' ru,
          HExtend f2 (Record ru),
          Record f2ru ~ HExtendR f2 (Record ru)
        )
    => UnionSymRec' False r1 f2 r2' f2ru where
    unionSR' _ r1 f2 r2' = (ul', ur')
       where (ul,ur) = unionSR r1 r2'
             ul' = f2 .*. ul
             ur' = f2 .*. ur

-- --------------------------------------------------------------------------
-- | Rearranges a record by labels. Returns the record r, rearranged such that
-- the labels are in the order given by ls. (LabelsOf r) must be a
-- permutation of ls.
hRearrange :: (HLabelSet ls, HRearrange ls r r') => Proxy ls -> Record r -> Record r'
hRearrange ls (Record r) = Record (hRearrange2 ls r)

{- | 'hRearrange'' is 'hRearrange' where ordering specified by the Proxy
argument is determined by the result type.

With built-in haskell records, these @e1@ and @e2@ have the same type:

> data R = R { x, y :: Int }
> e1 = R{ x = 1, y = 2}
> e2 = R{ y = 2, x = 1}

'hRearrange'' can be used to allow either ordering to be accepted:

> h1, h2 :: Record [ Tagged "x" Int, Tagged "y" Int ]
> h1 = hRearrange' $
>     x .=. 1 .*.
>     y .=. 2 .*.
>     emptyRecord
>
> h2 = hRearrange' $
>     y .=. 2 .*.
>     x .=. 1 .*.
>     emptyRecord

-}
hRearrange' r =
    let r' = hRearrange (labelsOf r') r
    in r'


class Rearranged r s t a b where
    -- @Iso (r s) (r t) (r a) (r b)@
    rearranged :: (Profunctor p, Functor f) => r a `p` f (r b) -> r s `p` f (r t)


{- | @Iso (Record s) (Record t) (Record a) (Record b)@

where @s@ is a permutation of @a@, @b@ is a permutation of @t@.
In practice 'sameLabels' and 'sameLength' are likely needed on both
sides of @rearranged@, to avoid ambiguous types.

An alternative implementation:

> rearranged x = iso hRearrange' hRearrange' x

-}
instance (la ~ LabelsOf a, lt ~ LabelsOf t,
          HRearrange la s a,
          HRearrange lt b t,
          HLabelSet la,
          HLabelSet lt)
  => Rearranged Record s t a b where
    rearranged = iso (hRearrange (Proxy :: Proxy la))
                     (hRearrange (Proxy :: Proxy lt))

{- | @Iso' (r s) (r a)@

where @s@ is a permutation of @a@ -}
rearranged' x = isSimple rearranged x

-- | Helper class for 'hRearrange'
class (HRearrange3 ls r r', LabelsOf r' ~ ls,
       SameLength ls r, SameLength r r')
      => HRearrange (ls :: [*]) r r' | ls r -> r', r' -> ls where
    hRearrange2 :: proxy ls -> HList r -> HList r'


instance (HRearrange3 ls r r', LabelsOf r' ~ ls,
        SameLength ls r, SameLength r r') => HRearrange ls r r' where
    hRearrange2 = hRearrange3

-- | same as HRearrange, except no backwards FD
class HRearrange3 (ls :: [*]) r r' | ls r -> r' where
    hRearrange3 :: proxy ls -> HList r -> HList r'

instance HRearrange3 '[] '[] '[] where
   hRearrange3 _ _ = HNil

instance (H2ProjectByLabels '[l] r rin rout,
          HRearrange4 l ls rin rout r',
          l ~ Label ll) =>
        HRearrange3 (l ': ls) r r' where
   hRearrange3 _ r = hRearrange4 (Proxy :: Proxy l) (Proxy :: Proxy ls) rin rout
      where (rin, rout) = h2projectByLabels (Proxy :: Proxy '[l]) r


-- | Helper class 2 for 'hRearrange'
class HRearrange4 (l :: *) (ls :: [*]) rin rout r' | l ls rin rout -> r' where
    hRearrange4 :: proxy l -> Proxy ls -> HList rin -> HList rout -> HList r'

instance (HRearrange3 ls rout r',
         r'' ~ (Tagged l v ': r'),
         ll ~ Label l) =>
        HRearrange4 ll ls '[Tagged l v] rout r'' where
   hRearrange4 _ ls (HCons lv@(Tagged v) _HNil) rout
        = HCons (Tagged v `asTypeOf` lv) (hRearrange3 ls rout)

-- | For improved error messages. XXX FieldNotFound
instance Fail (FieldNotFound l ()) =>
        HRearrange4 l ls '[] rout '[] where
   hRearrange4 _ _ _ _ = error "Fail has no instances"

-- | For improved error messages
instance Fail (ExtraField l) =>
          HRearrange3 '[] (Tagged l v ': a) '[] where
   hRearrange3 _ _ = error "Fail has no instances"


-- --------------------------------------------------------------------------
-- $lens
-- Lens-based setters/getters are popular. hLens packages up
-- 'hUpdateAtLabel' and 'hLookupByLabel'.
--
-- Refer to @examples/lens.hs@ and @examples/labelable.hs@ for examples.

-- | constraints needed to implement 'HLens'
type HLensCxt x r s t a b =
    (HasField x (r s) a,
     HUpdateAtLabel r x b s t,
     HasField x (r t) b,
     HUpdateAtLabel r x a t s,
     SameLength s t,
     SameLabels s t)

class HLensCxt x r s t a b => HLens x r s t a b
        | x s b -> t, x t a -> s, -- need to repeat fundeps implied by HLensCxt
          x s -> a, x t -> b where
    -- | @hLens :: Label x -> Lens (r s) (r t) a b@
    hLens :: Label x -> (forall f. Functor f => (a -> f b) -> (r s -> f (r t)))

instance HLensCxt r x s t a b => HLens r x s t a b where
  hLens lab f rec = fmap (\v -> hUpdateAtLabel lab v rec) (f (rec .!. lab))


{- | map over the values of a record. This is a shortcut for

  > \ f (Record a) -> Record (hMap (HFmap f) a)

[@Example@]

suppose we have a function that should be applied to every element
of a record:

>>> let circSucc_ x | x == maxBound = minBound | otherwise = succ x

>>> :t circSucc_
circSucc_ :: (Bounded a, Enum a, Eq a) => a -> a

Use a shortcut ('Fun') to create a value that has an appropriate 'ApplyAB' instance:

>>> let circSucc = Fun circSucc_ :: Fun '[Eq,Enum,Bounded] '()

Confirm that we got Fun right:

>>> :t applyAB circSucc
applyAB circSucc :: (Bounded b, Enum b, Eq b) => b -> b

>>> applyAB circSucc True
False

define the actual record:

>>> let r = x .=. 'a' .*. y .=. False .*. emptyRecord
>>> r
Record{x='a',y=False}

>>> hMapR circSucc r
Record{x='b',y=True}

-}
hMapR f r = applyAB (HMapR f) r

newtype HMapR f = HMapR f

instance (HMapCxt Record f x y, rx ~ Record x, ry ~ Record y)
      => ApplyAB (HMapR f) rx ry where
        applyAB (HMapR f) = hMapAux f

instance HMapAux HList (HFmap f) x y =>
    HMapAux Record f x y where
      hMapAux f (Record x) = Record (hMapAux (HFmap f) x)



-- --------------------------------------------------------------------------
-- | This instance allows creating a Record with
--
-- @hBuild 3 'a' :: Record '[Tagged "x" Int, Tagged "y" Char]@
instance (HReverse l lRev,
         HMapTaggedFn lRev l') => HBuild' l (Record l') where
  hBuild' l = hMapTaggedFn (hReverse l)

-- | serves the same purpose as 'hEnd'
hEndR :: Record a -> Record a
hEndR = id


-- | see 'hEndP'
instance (HRevAppR l '[] ~ lRev,
          HExtendRs lRev (Proxy ('[] :: [*])) ~ Proxy l1,
          l' ~ l1) => HBuild' l (Proxy l') where
  hBuild' _ = Proxy

{- | @'hEndP' $ 'hBuild' label1 label2@

is one way to make a Proxy of labels (for use with 'asLabelsOf'
for example). Another way is

@label1 .*. label2 .*. 'emptyProxy'@

-}
hEndP :: Proxy (xs :: [k]) -> Proxy xs
hEndP = id

type family HExtendRs (ls :: [*]) (z :: k) :: k
type instance HExtendRs (l ': ls) z = HExtendR l (HExtendRs ls z)
type instance HExtendRs '[] z = z

-- --------------------------------------------------------------------------

{- |

>>> let x :: Record '[Tagged "x" Int]; x = undefined
>>> let y :: Record '[Tagged "x" Char]; y = undefined
>>> :t hZip x y
hZip x y :: Record '[Tagged "x" (Int, Char)]

-}
instance (HZipRecord x y xy, SameLengths [x,y,xy])
      => HZip Record x y xy where
    hZip = hZipRecord

instance (HZipRecord x y xy, SameLengths [x,y,xy])
      => HUnzip Record x y xy where
    hUnzip = hUnzipRecord


#if __GLASGOW_HASKELL__ != 706
{- | Missing from ghc-7.6, because HZip Proxy instances interfere with HZip
HList instances.

a variation on 'hZip' for 'Proxy', where
the list of labels does not have to include Label
(as in @ts'@)

>>> let ts = Proxy :: Proxy ["x","y"]
>>> let ts' = Proxy :: Proxy [Label "x",Label "y"]
>>> let vs = Proxy :: Proxy [Int,Char]

>>> :t zipTagged ts Proxy
zipTagged ts Proxy :: Proxy '[Tagged "x" y, Tagged "y" y1]

>>> :t zipTagged ts vs
zipTagged ts vs :: Proxy '[Tagged "x" Int, Tagged "y" Char]


And and the case when hZip does the same thing:

>>> :t zipTagged ts' vs
zipTagged ts' vs :: Proxy '[Tagged "x" Int, Tagged "y" Char]

>>> :t hZip ts' vs
hZip ts' vs :: Proxy '[Tagged "x" Int, Tagged "y" Char]

-}
zipTagged :: (MapLabel ts ~ lts,
              HZip Proxy lts vs tvs)
      => Proxy ts -> proxy vs -> Proxy tvs
zipTagged _ _ = Proxy
#endif



class HZipRecord x y xy | x y -> xy, xy -> x y where
    hZipRecord :: Record x -> Record y -> Record xy
    hUnzipRecord :: Record xy -> (Record x,Record y)


instance HZipRecord '[] '[] '[] where
    hZipRecord _ _ = emptyRecord
    hUnzipRecord _ = (emptyRecord, emptyRecord)

instance HZipRecord as bs abss
       => HZipRecord (Tagged x a ': as) (Tagged x b ': bs) (Tagged x (a,b) ': abss) where
    hZipRecord (Record (Tagged a `HCons` as)) (Record (Tagged b `HCons` bs)) =
        let Record abss = hZipRecord (Record as) (Record bs)
        in Record (Tagged (a,b) `HCons` abss)
    hUnzipRecord (Record (Tagged (a,b) `HCons` abss)) =
        let (Record as, Record bs) = hUnzipRecord (Record abss)
        in (Record (Tagged a `HCons` as), Record (Tagged b `HCons` bs))


-- | instead of explicit recursion above, we could define HZipRecord in
-- terms of 'HZipList'. While all types are inferred, this implementation
-- is probably slower, so explicit recursion is used in the 'HZip' 'Record'
-- instance.
hZipRecord2 x y = hMapTaggedFn (hZipList (recordValues x) (recordValues y))
        `asLabelsOf` x `asLabelsOf` y

hUnzipRecord2 xy = let (x,y) = hUnzipList (recordValues xy)
                 in (hMapTaggedFn x `asLabelsOf` xy, hMapTaggedFn y `asLabelsOf` xy)


{- | similar to 'asTypeOf':

>>> let s0 = Proxy :: Proxy '["x", "y"]
>>> let s1 = Proxy :: Proxy '[Label "x", Label "y"]
>>> let s2 = Proxy :: Proxy '[Tagged "x" Int, Tagged "y" Char]

>>> let f0 r = () where _ = r `asLabelsOf` s0
>>> let f1 r = () where _ = r `asLabelsOf` s1
>>> let f2 r = () where _ = r `asLabelsOf` s2

>>> :t f0
f0 :: r '[Tagged "x" v, Tagged "y" v1] -> ()

>>> :t f1
f1 :: r '[Tagged "x" v, Tagged "y" v1] -> ()

>>> :t f2
f2 :: r '[Tagged "x" v, Tagged "y" v1] -> ()

-}
asLabelsOf :: (HAllTaggedLV x, SameLabels x y, SameLength x y) => r x -> s y -> r x
asLabelsOf = const
