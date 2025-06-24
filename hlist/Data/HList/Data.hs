{-# LANGUAGE CPP #-}

{- | Description: Data instances


'Data.Data.Data' instances for 'HListFlat' and 'Record' which pretend
to be flat data structures. The @Data@ instance for 'HList' gives a nested
structure.

NOTE: these instances do not work with ghc-7.8 with promoted
string (Symbol) labels because of
<https://ghc.haskell.org/trac/ghc/ticket/9111>

[@HList@]

The data instance for

> a :: HList '[Int, Double, b]

Looks like the same instance for

> type T b = (Int, (Double, (b, ())))


[@HListFlat@]

The Data instance for

> a :: Data b => HListFlat '[Int,Double,b]

will look like the Data instance for:

> data A b = A Int Double b


[@Record@]

For 'Record' similar ideas apply. An

> a :: Record '[ LVPair "x" Int, LVPair "y" Double ]

should behave like a:

> data A = A { x :: Int, y :: Double } deriving (Data)

Many unsafecoerces are necessary here because the Data class includes type
parameters @c@ that cannot be used in the class context for the instance.
Perhaps there is another way.

-}
module Data.HList.Data (
    -- * exports for type signatures/ haddock usage
    DataHListFlatCxt,
    DataRecordCxt,
    TypeRepsList(..),

    -- ** less likely to be used
    RecordLabelsStr(..),
    GfoldlK(..),
    GunfoldK(..),
    HListFlat(..),
    TypeablePolyK,
    ) where

import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.Record
import Data.HList.Variant
import Data.Data
import Data.HList.TIC
import Data.HList.TIP

-- for Typeable '[] and Typeable '(:) with ghc-7.6
import Data.Orphans ()

#if OLD_TYPEABLE
import Data.List
#endif

import Unsafe.Coerce


deriving instance Typeable (HList '[]) => Data (HList '[])
deriving instance
    (Data x,
     Data (HList xs),
     TypeablePolyK (x ': xs), -- for new typeable
     Typeable (HList (x ': xs) -- for old typeable
     )) => Data (HList (x ': xs))

deriving instance
    (TypeablePolyK xs,
     Typeable (HList xs),
     Data (HList xs)) => Data (TIP xs)
deriving instance
    (TypeablePolyK xs,
     Typeable (Variant xs),
     Data (Variant xs)) => Data (TIC xs)

-- | this data type only exists to have Data instance
newtype HListFlat a = HListFlat (HList a)

type DataHListFlatCxt na g a = (
        g ~ FoldRArrow a (HList a),
        HBuild' '[] g,
        Typeable (HListFlat a),
        TypeablePolyK a,
        HFoldl (GfoldlK  C) (C g) a (C (HList a)),

        HFoldr
            (GunfoldK C)
            (C g)
            (HReplicateR na ())
            (C (HList a)),

        HLengthEq a na,
        HReplicate na ())


-- | ghc-8.0.2 can't work out the type g,
-- in the 2nd argument of gfoldl. ghc <= 7.10
-- don't need it.
--
-- in `instance Data (HListFlat '[a,b,c])`
--
-- > g ~ (a -> b -> c -> HList '[a,b,c])
-- > g ~ GetG '[a,b,c] (HList '[a,b,c])
type family FoldRArrow (xs :: [*]) (r :: *)

type instance FoldRArrow '[] r = r
type instance FoldRArrow (x ': xs) r = x -> FoldRArrow xs r 


instance DataHListFlatCxt na g a => Data (HListFlat a) where
    gfoldl k z (HListFlat xs) = c3 $
                    hFoldl
                        (c1 (GfoldlK k))
                        (c2 (z hBuild))
                        xs
        where
              c1 :: forall c. GfoldlK c -> GfoldlK C
              c1 = unsafeCoerce

              c2 :: forall c. c g -> C g
              c2 = unsafeCoerce

              c3 :: forall c. C (HList a) -> c (HListFlat a)
              c3 = unsafeCoerce

    gunfold k z _ =
              c3 $ withSelf $ \self ->
                hFoldr
                    (c1 (GunfoldK k))
                    (c2 (z hBuild))
                    (hReplicate (hLength self) ())
        where
              withSelf :: forall t c. (t -> c t) -> c t
              withSelf x = x undefined

              c1 :: forall c. GunfoldK c -> GunfoldK C
              c1 = unsafeCoerce

              c2 :: forall c. c g -> C g
              c2 = unsafeCoerce

              c3 :: forall c. C (HList a) -> c (HListFlat a)
              c3 = unsafeCoerce

    dataTypeOf _ = hListFlatDataRep
    toConstr _   = hListFlatConRep

hListFlatDataRep = mkDataType "Data.HList.HList" [hListFlatConRep]
hListFlatConRep = mkConstr hListFlatDataRep "HListFlat" [] Prefix

type DataRecordCxt a =
    (Data (HListFlat (RecordValuesR a)),
            TypeablePolyK a,
            TypeRepsList (Record a),
            RecordValues a,
            RecordLabelsStr a)

instance DataRecordCxt a => Data (Record a) where
    gfoldl k z xs = c1 (gfoldl k z (HListFlat (recordValues xs)))
        where
            c1 :: forall c. c (HListFlat (RecordValuesR a)) -> c (Record a)
            c1 = unsafeCoerce

    gunfold k z con = c1 (gunfold k z con)
        where
            -- LVPair and Record are newtypes, so this should be safe...
            c1 :: forall c. c (HListFlat (RecordValuesR a)) -> c (Record a)
            c1 = unsafeCoerce

    dataTypeOf x = snd (recordReps (recordLabelsStr x))
    toConstr x = fst (recordReps (recordLabelsStr x))


recordReps fields =
    let c = mkConstr d "Record" fields Prefix
        d = mkDataType "Data.HList.Record" [c]
    in (c,d)



class RecordLabelsStr (xs :: [*]) where
    recordLabelsStr :: Record xs -> [String]

instance RecordLabelsStr '[] where
    recordLabelsStr _ = []
instance (RecordLabelsStr xs,
          ShowLabel x) => RecordLabelsStr (Tagged x t ': xs) where
    recordLabelsStr _ = showLabel (Label :: Label x) :
                            recordLabelsStr (undefined :: Record xs)

{- |

This alternative option works too, but for whatever reason
splitting up recordLabelsStr and recordLabels into two functions
means that a type annotation is needed on the 3, which is not
necessary with the above recordLabelsStr (ghc-7.6.3)

> recordLabelsStr2 (recordLabels (((Label :: Label "x") .=. 3 .*. emptyRecord )))

-}
class RecordLabelsStr2 (xs :: [k]) where
    recordLabelsStr2 :: proxy xs -> [String]

instance RecordLabelsStr2 '[] where
    recordLabelsStr2 _ = []
instance (RecordLabelsStr2 xs,
          ShowLabel x) => RecordLabelsStr2 (x ': xs) where
    recordLabelsStr2 _ = showLabel (Label :: Label x) :
                            recordLabelsStr2 (Proxy :: Proxy xs)


-- | use only with @instance Data (HList a)@. This is because the HFoldl
-- context cannot be written for a @c@ that only appears in the method
-- 'gfoldl'.
data C a

-- typeable isntances... either hand written or derived when possible
#if !OLD_TYPEABLE
deriving instance Typeable Record
deriving instance Typeable HList
deriving instance Typeable HListFlat
deriving instance Typeable Variant
deriving instance Typeable TIC
deriving instance Typeable TIP

-- orphans
deriving instance Typeable 'HZero
deriving instance Typeable 'HSucc

#else
instance TypeRepsList (Record xs) => Typeable (HList xs) where
   typeOf x = mkTyConApp (mkTyCon3 "HList" "Data.HList.HList" "HList")
                [ tyConList (typeRepsList (Record x)) ]

instance (TypeRepsList (Record xs)) => Typeable (Record xs) where
  typeOf x = mkTyConApp (mkTyCon3 "HList" "Data.HList.Record" "Record")
                [ tyConList (typeRepsList x) ]

instance TypeRepsList (Record xs) => Typeable (Variant xs) where
  typeOf _ = mkTyConApp (mkTyCon3 "HList" "Data.HList.Variant" "Variant")
                [ tyConList (typeRepsList (error "Data.HList.Data:Typeable Variant" :: Record xs)) ]

instance Typeable (Variant xs) => Typeable (TIC xs) where
  typeOf (TIC xs) = mkTyConApp (mkTyCon3 "HList" "Data.HList.TIC" "TIC")
                      [typeOf xs]

instance Typeable (HList xs) => Typeable (TIP xs) where
  typeOf (TIP xs) = mkTyConApp (mkTyCon3 "HList" "Data.HList.TIP" "TIP")
                      [typeOf xs]

instance ShowLabel sy => Typeable1 (Tagged sy) where
  typeOf1 _ = mkTyConApp
        (mkTyCon3 "HList" "Data.HList.Data" (showLabel (Label :: Label sy)))
        []

instance (ShowLabel sy, Typeable x) => Typeable (Tagged sy x) where
  typeOf _ = mkTyConApp
            (mkTyCon3 "GHC" "GHC.TypeLits" (showLabel (Label :: Label sy)))
            [mkTyConApp (mkTyCon3 "HList" "Data.HList.Record" "=") [],
                    typeOf (error "Data.HList.Data:Typeable Tagged" :: x)
                    ]


instance Typeable (HList a) => Typeable (HListFlat a) where
    typeOf _ = mkTyConApp (mkTyCon3 "HList" "Data.HList.Data" "HListFlat")
            [typeOf (error "Typeable HListFlat" :: HList a)]

-- pretty-prints sort of like a real list
tyConList xs = mkTyConApp open ( intersperse comma xs ++ [close] )
    where
    open = mkTyCon3 "GHC" "GHC.TypeLits" "["
    close = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" "]") []
    comma = mkTyConApp (mkTyCon3 "GHC" "GHC.TypeLits" ",") []
#endif





class TypeRepsList a where
  typeRepsList :: a -> [TypeRep]


instance (TypeRepsList (HList xs)) => TypeRepsList (Record xs) where
  typeRepsList (Record xs) = typeRepsList xs

instance (TypeRepsList (HList xs), Typeable x) => TypeRepsList (HList (x ': xs)) where
  typeRepsList (~(x `HCons` xs))
        = typeOf x : typeRepsList xs

instance TypeRepsList (HList '[]) where
  typeRepsList _ = []



-- | wraps up the first argument to 'gfoldl'
data GfoldlK c where
    GfoldlK :: (forall d b . Data d => c (d -> b) -> d -> c b) -> GfoldlK c

instance (Data d, (c (d -> b), d) ~ x, c b ~ y) =>
        ApplyAB (GfoldlK c) x y where
    applyAB (GfoldlK f) (u,v) = f u v


data GunfoldK c where
    GunfoldK :: (forall b r. Data b => c (b -> r) -> c r) -> GunfoldK c

instance (Data b, x ~ (t, c (b -> r)), y ~ c r) =>
        ApplyAB (GunfoldK c) x y where
    applyAB (GunfoldK f) (_, u) = f u


