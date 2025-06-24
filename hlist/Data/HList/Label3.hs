{-# LANGUAGE CPP #-}

{- |

   Description : namespaced labels

   The HList library

   (C) 2004-2006, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A model of labels as needed for extensible records. As before,
   all the information about labels is recorded in their type, so
   the labels of records may be purely phantom. In general,
   Labels are exclusively type-level entities and have no run-time
   representation.

   Record labels are triplets of type-level naturals, namespace,
   and description. The namespace part helps avoid confusions between
   labels from different Haskell modules. The description is
   an arbitrary nullary type constructor.

   For the sake of printing, the description is required to be the
   instance of Show. One must make sure that the show functions does
   not examine the value, as descr is purely phantom. Here's an
   example of the good Label description:

   >     data MyLabelDescr; instance Show MyLabelDescr where show _ = "descr"

   which are automated by makeLabels3 from "Data.HList.MakeLabel".

   This model even allows the labels in a record to belong to different
   namespaces. To this end, the model employs the predicate for type
   equality.
-}

module Data.HList.Label3 where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import GHC.TypeLits

{- $setup

>>> let label3 = Label :: Label (Lbl HZero () ())
>>> let label6 = Label :: Label "6"

-}

data Lbl (x :: HNat) (ns :: *) (desc :: *)  -- labels are exclusively type-level entities
#if OLD_TYPEABLE
instance (ShowLabel x) => Typeable2 (Lbl x) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "HList" "Data.HList.Label3" "Lbl")
    [mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" (showLabel (Label :: Label x)))
      []]
#endif

type instance ZipTagged (Lbl ix ns n ': ts) (v ': vs) = Tagged (Lbl ix ns n) v ': ZipTagged ts vs

instance (Label t ~ Label (Lbl ix ns n)) => SameLabels (Label t) (Lbl ix ns n)

-- * Public constructors for labels

-- | Construct the first label
firstLabel :: ns -> desc -> Label (Lbl HZero ns desc)
firstLabel _ _ = Label


-- | Construct the next label
nextLabel :: Label (Lbl x ns desc) -> desc' -> Label (Lbl (HSucc x) ns desc')
nextLabel _ _ = Label


-- | Equality on labels (descriptions are ignored)
-- Use generic instance
{-
instance ( HEq x x' b
         , HEq ns ns' b'
         , bres ~ HAnd b b'
         )
      =>   HEq (Lbl x ns desc) (Lbl x' ns' desc') bres
-}

-- | Show label

instance Show desc => ShowLabel (Lbl x ns desc) where
  showLabel = show . getd
      where getd :: Label (Lbl x ns desc) -> desc -- for the sake of Hugs
            getd = error "Data.HList.Label3 desc"

instance Show desc => Show (Label (Lbl x ns desc))
 where
  show = show . getd
      where getd :: Label (Lbl x ns desc) -> desc -- for the sake of Hugs
            getd = error "Data.HList.Label3 desc"



{- |

If possible, Label is left off:

>>> let q = label3 .*. label3 .*. emptyProxy
>>> :t q
q :: Proxy '[Lbl 'HZero () (), Lbl 'HZero () ()]

-}
instance HExtend (Label (Lbl n ns desc)) (Proxy (Lbl n' ns' desc' ': xs)) where
    type HExtendR (Label (Lbl n ns desc)) (Proxy (Lbl n' ns' desc' ': xs))
                = Proxy (Lbl n ns desc ': Lbl n' ns' desc' ': xs)
    (.*.) _ _ = Proxy

{- | Mixing two label kinds means we have to include 'Label':

>>> let r = label3 .*. label6 .*. emptyProxy
>>> :t r
r :: Proxy '[Label (Lbl 'HZero () ()), Label "6"]

-}
instance HExtend (Label (Lbl n ns desc)) (Proxy (x ': xs :: [Symbol])) where
    type HExtendR (Label (Lbl n ns desc)) (Proxy (x ': xs))
              = Proxy (Label (Lbl n ns desc) ': MapLabel (x ': xs))
    (.*.) _ _ = Proxy

{- | Mixing two label kinds means we have to include 'Label':

>>> let s = label6 .*. label3 .*. emptyProxy
>>> :t s
s :: Proxy '[Label "6", Label (Lbl 'HZero () ())]

-}
instance HExtend (Label (y :: Symbol)) (Proxy ((x :: *) ': xs)) where
    type HExtendR (Label (y :: Symbol)) (Proxy (x ': xs))
          = Proxy (Label y ':  (MapLabel (x ': xs)))
    (.*.) _ _ = Proxy

instance HExtend (Label (y :: Symbol)) (Proxy ((x :: Nat) ': xs)) where
    type HExtendR (Label (y :: Symbol)) (Proxy (x ': xs))
          = Proxy (Label y ':  (MapLabel (x ': xs)))
    (.*.) _ _ = Proxy

instance HExtend (Label (y :: Nat)) (Proxy ((x :: *) ': xs)) where
    type HExtendR (Label (y :: Nat)) (Proxy (x ': xs))
          = Proxy (Label y ':  (MapLabel (x ': xs)))
    (.*.) _ _ = Proxy

instance HExtend (Label (y :: Nat)) (Proxy ((x :: Symbol) ': xs)) where
    type HExtendR (Label (y :: Nat)) (Proxy (x ': xs))
          = Proxy (Label y ':  (MapLabel (x ': xs)))
    (.*.) _ _ = Proxy

-- | similar to Data.HList.Record.Labels1, but avoids producing Label (Label x)
type family MapLabel (xs :: [k]) :: [*]
type instance MapLabel '[] = '[]
#if NO_CLOSED_TF
-- if we can't have any overlap in the TF (ghc-7.6), cover the Label3/Label6 case
type instance MapLabel ((x :: Symbol) ': xs) = Label x ': MapLabel xs
type instance MapLabel (Lbl n ns desc ': xs) = Label (Lbl n ns desc) ': MapLabel xs
type instance MapLabel (Label x ': xs) = Label x ': MapLabel xs
#else
type instance MapLabel (x ': xs) = AddLabel x ': MapLabel xs
type family AddLabel (x :: k) :: * where
  AddLabel (Label x) = Label x
  AddLabel x = Label x
#endif
