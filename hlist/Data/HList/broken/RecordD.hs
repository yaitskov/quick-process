{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  UndecidableInstances #-}

{- |
Yet another representation of records: records as TIC (type-indexed
collections), or, to be precise, records are lists of objects
that support the 'Fieldish' interface. So, we can build records like that

> data Name = Name String String
> newtype Salary = S Float
> data Dept = D String Int
> person = (Name "Joe" "Doe") .*. (S 1000) .*. (Dept "CIO" 123) .*. emptyRec

-}

module RecordD where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import qualified Data.HList.Record
import Data.HList.Record (HLabelSet, HasField(..))

-- for the test
import Data.HList.TypeEqO

instance (HBool b, TypeEq x y b) => HEq x y b

-- | Define the interface of fields: basically a thing with a label
-- and injection and projection methods
class Fieldish l v | l -> v where
    fromField :: l -> v
    toField :: v -> l

newtype Record r = Record r


-- | Build a record: a record is an HList of data items, provided
--
-- (1) the types of the data items are unique
--
-- (2) each item satsifies the interface 'Fieldish'

mkRecord :: (HLabelSet r, AllFieldish r)  => r -> Record r
mkRecord = Record



-- | Build an empty record

emptyRecord = mkRecord HNil


-- | make sure that all elements of an HList are Fieldish
class AllFieldish r
instance AllFieldish HNil
instance (Fieldish e v, AllFieldish r) => AllFieldish (HCons e r)

-- --------------------------------------------------------------------------

-- * Show

-- | A Show instance to appeal to normal records. Assume each Fieldish
-- is showable

instance ShowComponents r => Show (Record r)
 where
  show (Record r) =  "Record{"
                  ++ showComponents "" r
                  ++ "}"

class ShowComponents l
 where
  showComponents :: String -> l -> String

instance ShowComponents HNil
 where
  showComponents _ HNil = ""

instance ( Show f, ShowComponents r )
      =>   ShowComponents (HCons f r)
 where
  showComponents comma (HCons f r)
     =  comma
     ++ show f
     ++ showComponents "," r


-- --------------------------------------------------------------------------
-- * Extension for records

instance (AllFieldish (HCons f r), HLabelSet (HCons f r))
    => HExtend f (Record r) (Record (HCons f r))
 where
  hExtend f (Record r) = mkRecord (HCons f r)

-- --------------------------------------------------------------------------
-- Record concatenation

instance ( HLabelSet r''
         , AllFieldish r''
         , HAppend r r' r''
         )
    => HAppend (Record r) (Record r') (Record r'')
 where
  hAppend (Record r) (Record r') = mkRecord (hAppend r r')


-- --------------------------------------------------------------------------
-- * Lookup operation

instance (HEq l l' b, HasField' b l (HCons l' r) v)
    => HasField l (Record (HCons l' r)) v where
    hLookupByLabel l (Record r@(HCons f' _)) = hLookupByLabel' (hEq l f') l r

class HasField' b l r v | b l r -> v where
    hLookupByLabel':: b -> l -> r -> v

instance Fieldish l v => HasField' HTrue l (HCons l r) v where
    hLookupByLabel' _ _ (HCons f _) = fromField f
instance HasField l (Record r) v => HasField' HFalse l (HCons fld r) v where
    hLookupByLabel' _ l (HCons _ r) = hLookupByLabel l (Record r)


-- some tests
data Name      = Name String String deriving Show
newtype Salary = S Float deriving Show
data Dept      = D String Int deriving Show

-- could be derived automatically, like Typeable...
instance Fieldish Name (String,String) where
    fromField (Name s1 s2) = (s1,s2)
instance Fieldish Salary Float where
    fromField (S n) = n
instance Fieldish Dept (String,Int) where
    fromField (D s n) = (s,n)

infixr 2 .*.
(.*.) :: HExtend e l l' => e -> l -> l'
(.*.) =  hExtend
infixr 3 .!.
r .!. l =  hLookupByLabel l r

person = (Name "Joe" "Doe") .*. (S 1000) .*. (D "CIO" 123) .*. emptyRecord

test1 = show person
-- "Record{Name \"Joe\" \"Doe\",S 1000.0,D \"CIO\" 123}"
-- only the type of the label matters, not the contents
test2 = person .!. (Name undefined undefined)
-- ("Joe","Doe")
test3 = person .!. (undefined::Salary)
-- 1000.0
test5 = person .!. (D "xxx" 111)
-- ("CIO",123)
