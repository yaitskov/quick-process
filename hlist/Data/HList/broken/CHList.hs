{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}




-- XXX Updates are delayed...
-- | Potentially infinite, open, statically constrained HLists...

module CHList where

-- Nat is not strictly necessary, but good for error-checking...
-- Mapping from Nats to {HNothing,HJust x}
-- The end of the list is HNothing with the smallest index

class Nat n => CHList l n r | l n -> r where
    chel :: l -> n -> r
    chel = undefined

-- Default `cut-off' instance
instance (Nat n, TypeCast r HNothing) => CHList l n r


-- The first two CHList

data LEmpty = LEmpty                    -- and that is it...

data L1 = L1
instance CHList L1 Z (HJust Int) where
    chel _ _ = HJust 0
instance CHList L1 (S Z) (HJust Int) where
    chel _ _ = HJust 1


-- Convert CHList to HList, starting from the N-th element
class CHL2HL l n r | l n -> r where
    chl_to_hl_n :: l -> n -> r

instance (CHList l n el, CHL2HL (CHL2HL' l el) n r)
    => CHL2HL l n r where
    chl_to_hl_n l n = chl_to_hl_n (CHL2HL' l (chel l n)) n

data CHL2HL' l el = CHL2HL' l el

instance CHL2HL (CHL2HL' l HNothing) n HNil where
    chl_to_hl_n _ _ = HNil

instance CHL2HL l (S n) r => CHL2HL (CHL2HL' l (HJust a)) n (HCons a r) where
    chl_to_hl_n (CHL2HL' l (HJust x)) n = HCons x (chl_to_hl_n l (S n))

chl_to_hl l = chl_to_hl_n l Z

test1 = show $ chl_to_hl L1


-- List functions

-- It is a type error to apply it to an empty list
cl_head l = fromHJust (chel l Z)

test_h1 = cl_head L1 -- 0
-- the following is a type error
-- test_h2 = cl_head LEmpty

newtype CLTail l = CLTail l
instance (Nat n, CHList l (S n) r) => CHList (CLTail l) n r where
    chel (CLTail l) n = chel l (S n)

-- ensure the list to take the tail of is non-empty
cl_tail :: CHList l Z (HJust e) => l -> CLTail l
cl_tail l = CLTail l

test_h3 = cl_head (cl_tail L1) -- 1

-- would be error: tail of empty list
-- test_t1 = cl_tail LEmpty

-- Can't take head of the empty list
-- test_h4 = cl_head (cl_tail (cl_tail L1))

data CLCons a l = CLCons a l
cl_cons = CLCons

instance CHList (CLCons a l) Z (HJust a) where
    chel (CLCons x _ ) _ = HJust x

instance (Nat n, CHList l n r) => CHList (CLCons a l) (S n) r where
    chel (CLCons _ l) (S n) = chel l n


test_c1 = cl_head (cl_tail (cl_cons ((-2)::Int) (cl_cons ((-1)::Int) L1)))

-- and we can construct (heterogeneous) lists with the regular list API
test_c2 = cl_head (cl_tail (cl_cons () (cl_cons True LEmpty)))

test_c3 = show $ chl_to_hl $ cl_cons () L1


-- Mapping
data CLMap f l = CLMap f l
cl_map = CLMap

instance (Nat n, CHList l n r', Apply (HJust f) r' r)
    => CHList (CLMap f l) n r where
    chel (CLMap f l) n = apply (HJust f) (chel l n)

instance Apply (HJust f) HNothing HNothing where
    apply _ _ = HNothing
instance Apply f x r => Apply (HJust f) (HJust x) (HJust r) where
    apply (HJust f) (HJust x) = HJust (apply f x)


-- Looks just like the regular map application...
test_m1 = cl_map (succ :: Int->Int) L1
test_m2 = show $ chl_to_hl test_m1

-- Notice the similarity with the following. The difference is the case of l1!
test_m1' = map (succ :: Int->Int) l1
    where l1 = [0,1]

-- Folding
data CLFold f z = CLFold f z
cl_fold f z l = apply (CLFold f z) (l,Z,chel l Z)


instance Apply (CLFold f z) (l,n,HNothing) z where
    apply (CLFold _ z) _ = z

instance (Apply f (x,z') r, CHList l (S n) e,
          Apply (CLFold f z) (l,(S n),e) z', Nat n)
    => Apply (CLFold f z) (l, n, HJust x) r where
    apply op@(CLFold f z) (l,n,HJust x) =
        apply f (x,apply op (l,S n,chel l (S n)))


test_f1 = cl_fold (uncurry ((+)::Int->Int->Int)) (0::Int) L1


-- Taking
data CLTake n l = CLTake n l
cl_take = CLTake


data PLEQ = PLEQ
-- PLEQ x y ==> HTrue if x <= y
instance Apply PLEQ (Z,Z) HTrue
instance Apply PLEQ (Z,S y) HTrue
instance Apply PLEQ (S x,Z) HFalse
instance Apply PLEQ (x,y) r => Apply PLEQ (S x,S y) r

data IF = IF
instance Apply IF (HTrue,x,y) x where
    apply _ (_,x,y) = x
instance Apply IF (HFalse,x,y) y where
    apply _ (_,x,y) = y


instance (Apply PLEQ (S m,n) bf, CHList l m x, Apply IF (bf,x,HNothing) r)
    => CHList (CLTake n l) m r where
    chel (CLTake n l) m = apply IF (undefined::bf, chel l m, HNothing)


-- Infinite lists

data LNats = LNats  -- lists of successive numerals
instance Nat n => CHList LNats n (HJust n) where
    chel _ n = HJust n

test_nat1 = cl_head (cl_tail LNats)
test_nat2 = cl_take four LNats
test_nat3 = show $ chl_to_hl test_nat2


data Add = Add
instance Nat n => Apply Add (Z,n) n
instance (Nat n, Nat m, Apply Add (n,m) r) => Apply Add (S n, m) (S r)

-- Other infinite lists can be defined by mapping over LNats

data Mul2 = Mul2
instance (Nat n, Apply Add (n,n) r) => Apply Mul2 n r

l2nats = cl_map Mul2 LNats -- double Nats
test_nn = show $ chl_to_hl $ cl_take four l2nats


data LFibs = LFibs  -- lists of Fibonacci numbers
instance CHList LFibs Z (HJust (S Z))
instance CHList LFibs (S Z) (HJust (S Z))
instance (Nat n, CHList LFibs (S n) (HJust e1), CHList LFibs n (HJust e2),
          Apply Add (e1,e2) r)
    => CHList LFibs (S (S n)) (HJust r)

test_fib = show $ chl_to_hl $ cl_take five LFibs
-- "HCons S Z (HCons S Z (HCons S S Z (HCons S S S Z 
--       (HCons S S S S S Z HNil))))"



-- Infinitely extensible lists

data LCyc = LCyc

instance (Nat n, CHList LCyc n r) => CHList LCyc (S (S n)) r where
    chel l (S (S n)) = chel l n

instance CHList LCyc Z (HJust ()) where
    chel _ _ = HJust ()

test_cyc = show $ chl_to_hl $ cl_take five LCyc

-- one instance: "HNil"
-- two instances: "HCons () HNil"

{-
instance CHList LCyc (S Z) (HJust Bool) where
    chel _ _ = HJust True
						 
-- "HCons () (HCons True (HCons () (HCons True (HCons () HNil))))"

instance CHList LCyc (S (S Z)) (HJust Bool) where
    chel _ _ = HJust False

-- "HCons () (HCons True (HCons False (HCons True (HCons False HNil))))"

instance CHList LCyc (S (S (S Z))) (HJust Char) where
    chel _ _ = HJust 'a'

test_cyc1 = show $ chl_to_hl $ cl_take seven LCyc
-}


-- test_cyc1
-- "HCons () (HCons True (HCons False (HCons 'a' (HCons False (HCons 'a' (HCons False HNil))))))"


-- Statically constrained lists
-- A constraint is a binary predicate, applied to two consecutive
-- CHList elements. The constraint is satisfied if the predicate
-- returns HTrue
-- For simplicity, we declare a class of lists whose elements are
-- non-decreasing numerals

data CK l = CK l

class (Nat n, Apply (CK l) (n,r) HTrue) => C1 l n r | l n -> r where
    chel1 :: l -> n -> r
    chel1 = undefined
-- Default `cut-off' instance
instance (Nat n, TypeCast r HNothing, Apply (CK l) (n, r) HTrue) => C1 l n r

instance Apply (CK l) (Z,r) HTrue
instance Apply (CK l) (S n,HNothing) HTrue
instance (C1 l n (HJust ep), Apply PLEQ (ep,e) r)
    => Apply (CK l) (S n,HJust e) r


data LC1 = LC1

instance C1 LC1 Z (HJust Z)
instance C1 LC1 (S Z) (HJust Z)
instance C1 LC1 (S (S Z)) (HJust (S (S (S Z))))

-- The following is the type error
-- instance C1 LC1 (S (S (S Z))) (HJust Z)

data LC2 = LC2
instance C1 LC2 Z (HJust Z)
          -- this constraint suggested by the typechecker
instance (Apply PLEQ (r, S (S r)) HTrue,
          C1 LC2 n (HJust r))
    => C1 LC2 (S n) (HJust (S (S r)))


newtype C1toCL cl = C1toCL cl

instance C1 cl n r => CHList (C1toCL cl) n r where
    chel (C1toCL l) n = chel1 l n

test_cl1 = show $ chl_to_hl $ C1toCL LC1

test_cl2 = show $ chl_to_hl $ cl_take five (C1toCL LC2)





-- A data type of heterogenous lists with whose elements are successively
-- increasing type-level naturals. These lists can be infinite.
-- In this simple case, we can get by without all the above machinery.
-- However, if the static constraint is non-decreasing order of elements,
-- we no longer able to express it that simply.
data SeqI a = ConsI a (SeqI (S a)) | NilI

seqi1 = ConsI Z (ConsI (S Z) NilI)

-- seqi2 = ConsI Z (ConsI Z NilI)

seqi_inf :: n -> SeqI n
seqi_inf n = ConsI n (seqi_inf (S n))

seqi3 = seqi_inf (S Z)



-- The standard HList stuff, included here for completeness

data Z = Z
newtype S a = S a

instance Show Z where show _ = "Z"
instance Show n => Show (S n) where show _ = "S " ++ show (undefined::n)

class Nat a                             -- Kind of natural numbers
instance Nat Z
instance Nat a => Nat (S a)

four = S $ S $ S $ S Z                  -- A few sample numbers
five = S $ four
seven = S $ S $ five


data HTrue
data HFalse

data HNothing = HNothing
newtype HJust x = HJust x
fromHJust (HJust x) = x                 -- this is the total function!

data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show


class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

-- A heterogeneous apply operator

class Apply f a r | f a -> r where
  apply :: f -> a -> r
  apply = undefined

-- Normal function application
instance Apply (x -> y) x y where
  apply f x = f x
