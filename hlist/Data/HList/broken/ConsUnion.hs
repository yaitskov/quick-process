{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

-- Updates delayed: need wildcard type comparison, to be implemented later

{- |

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel

Creating a regular (homogeneous) Haskell list by consing values of
generally different types. The consing operation builds a union as it
adds more elements to the list.

The operations consEither, nilEither are generalizations of ((:),[]).
In fact, the former reduce to the latter in the case of homogeneous
lists.

Our building of unions is efficient: if the new element is already
in the union, we inject it into the existing uniuon rather than
extend the union: see the tests te5-te7 below.

Future work, quoting Ralf:
``It looks like consEither is the deforested version of an
operation to take an HList and to return a non-HList. Probably both
are worth having, and the inverse operation of taking a homogenous
list with potentially an Either on the element type to return a plain
HList. This could be a new theme in the HList library -- all around
heterogeneous/homogeneous mediation.''

-}

module Data.HList.ConsUnion (NilEither, nilEither,
                  consEither,

                  -- Union injection/projection
                  TSame, TNone, TContains(..),
                  tsearch,
                  downCast ) where

import Data.HList.FakePrelude (HNothing(..), HJust(..))
import Data.HList.Record
import Data.HList.RecordAdv

-- List constructors that union as well

data NilEither
nilEither :: NilEither
nilEither = undefined :: NilEither

class ConsEither h t l | h t -> l
 where
  consEither :: h -> t -> l

instance ConsEither e  NilEither [e]
 where
  consEither h _ = [h]

instance (TypeEqInc eu e f, ConsEither' f e eu l)
    => ConsEither e [eu] l where
  consEither h t = consEither' (tsearch (undefined::eu) (undefined::e)) h t


class ConsEither' f e eu l | f e eu -> l where
    consEither' :: f -> e -> [eu] -> l

instance ConsEither' TNone e eu [Either e eu] where
    consEither' _ h t = Left h : map Right t

instance ConsEither' TSame e e [e] where
    consEither' _ h t = h : t

instance ConsEither' (TContains e eu) e eu [eu] where
    consEither' (TContains inj _) h t = inj h : t


-- Compare the type t with the type tu and return:
--  TSame -- if t = tu
--  TContains t tu -- if tu is the type Either tul tur and
--                      either tul or tur is or includes t
--  TNone -- otherwise

data TSame                              -- ^ outcomes of the comparison
data TContains t tu = TContains (t->tu)       -- injection function
                                (tu->Maybe t) -- projection function
data TNone

class TypeEqInc tu t res | tu t -> res where
    tsearch :: tu -> t -> res
    tsearch = undefined


instance TypeEqInc tu tu TSame
instance (IsEither tu atu, TypeEqInc' atu t res)
    => TypeEqInc tu t res
  where
    tsearch _ = tsearch' (undefined::atu)

class TypeEqInc' atu t res | atu t -> res where
    tsearch' :: atu -> t -> res
    tsearch' = undefined

-- instance TypeEqInc' (TOther tu)  t TNone
instance (TypeEqInc tul t atul, TypeEqInc tur t atur,
          TypeEqInc'' atul atur tul tur res)
    => TypeEqInc' (Either tul tur) t res where
    tsearch' _ t = tsearch'' (tsearch (undefined::tul) t)
                               (tsearch (undefined::tur) t)
                               (undefined::tul)
                               (undefined::tur)

class TypeEqInc'' atul atur tul tur res | atul atur tul tur -> res where
    tsearch'' :: atul -> atur -> tul -> tur -> res
    tsearch'' = undefined

instance TypeEqInc'' TNone TNone tul tur TNone

instance TypeEqInc'' TSame TNone t tur (TContains t (Either t tur)) where
    tsearch'' _ _ _ _ = TContains Left (either Just (const Nothing))

instance TypeEqInc'' TNone TSame tul t (TContains t (Either tul t)) where
    tsearch'' _ _ _ _ = TContains Right (either (const Nothing) Just)

instance TypeEqInc'' (TContains t tul) TNone tul tur
    (TContains t (Either tul tur)) where
    tsearch'' (TContains inj prj)  _ _ _ =
        TContains (Left . inj) (either prj (const Nothing))

instance TypeEqInc'' TNone (TContains t tur) tul tur
    (TContains t (Either tul tur)) where
    tsearch'' _ (TContains inj prj) _ _ =
        TContains (Right . inj) (either (const Nothing) prj)



instance (IsRecord tu atu, IsRecord t at, TypeEqIncR atu at res)
    => TypeEqInc' (TOther tu) t res where
    tsearch' _ _ = tsearchR (undefined::atu) (undefined::at)


class TypeEqIncR atu at res | atu at -> res where
    tsearchR :: atu -> at -> res
    tsearchR = undefined

instance TypeEqIncR (Record ru) (TOther b) TNone
instance TypeEqIncR (TOther a)  b TNone


instance (RecordEquiv' (Record ru -> r12)
                                   (Record r -> r21)
                                   b,
                      NarrowM r ru r21,
                      NarrowM ru r r12,
          RecordEquiv ru r b, TypeEqIncR' b res)
  -- For some reason the compiler needs all the above constraints, rather
  -- than the two below. Why does it need to `inline' the
  -- RecordEquiv ru r b constraint? Seems like a bug...
  -- (RecordEquiv ru r b, TypeEqIncR' b res)
    => TypeEqIncR (Record ru) (Record r) res where
    tsearchR ru r = tsearchR' (equivR ru r)

class TypeEqIncR' pjs res | pjs -> res where
    tsearchR' :: pjs -> res
    tsearchR' = undefined

instance TypeEqIncR' HNothing TNone

instance TypeEqIncR' (HJust (ru->r,r->ru)) (TContains r ru) where
    tsearchR' (HJust (pj,ij)) = TContains ij (Just . pj)



-- Check to see if t is an Either type or a record type
data TOther t
class IsEither t res | t -> res
instance IsEither (Either t1 t2) (Either t1 t2)
instance res ~ (TOther t) => IsEither t res

class IsRecord t res | t -> res
instance IsRecord (Record t) (Record t)
instance res ~ (TOther t) => IsRecord t res

-- A few tests of consEither

-- consEither should act as a regular cons for homogeneous lists
te1 = consEither () nilEither
te2 = consEither () (consEither () nilEither)
-- [(),()]

te3 = consEither True (consEither () nilEither)
-- [Left True,Right ()]

te4 = consEither 'a' (consEither True (consEither () nilEither))
-- [Left 'a',Right (Left True),Right (Right ())]

te5 = consEither () (consEither True (consEither () nilEither))
-- [Right (),Left True,Right ()]

te6 = consEither 'a' (consEither True (consEither False nilEither))
-- [Left 'a',Right True,Right False]

te7 = consEither True (consEither True (consEither () nilEither))
-- [Left True,Left True,Right ()]


-- |
-- Down-cast a value of a union type to a summand type.
-- Make sure that the summand type occurs once at least.

class DownCast u s
  where
    downCast :: u -> Maybe s

instance (TypeEqInc u s au, DownCast' au u s) => DownCast u s where
    downCast = downCast' (tsearch (undefined::u) (undefined::s))


class DownCast' au u s | au s -> u where
    downCast' :: au -> u -> Maybe s

instance DownCast' TSame s s where
    downCast' _ = Just

instance DownCast' (TContains s u) u s where
    downCast' (TContains _ prj) = prj


--
-- The following is deliberately omitted. That means attempting to
-- project a type that is not in the union should give a type error.
--
-- instance DownCast' TNone s s
--

