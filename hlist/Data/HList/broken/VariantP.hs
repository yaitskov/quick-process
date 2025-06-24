{- |
Modeling of an extensible, recursive sum datatype (recursive open union)

>  data List a = Nil | Cons a (List a)

which is later extended with two more variants,

>  ... | Unit a | App (List a) (List a)

Our goals:

  * any function that accepts the extended data list
    must also accept the unextended list.

  * it should be possible to `extend' the function that operates
    on plain lists to operate on extended list. We should be able
    to reuse as much of the old code as possible.

It seems, we have achieved our goals.

/Method/: duality rules! Curry-Howard correspondence and logic
give the wonderful guidance. In particular, we take advantage
of the fact that the deMorgan law

>     NOT (A | B) -> (NOT A & NOT B)

holds both in classical, and, more importantly, intuitionistic
logics. Our encoding of sums is the straightforward Curry-Howard
image of the law.

Note, the code below has no type classes, no type-level programming
or any other type hacking. In fact, there are no type annotations,
type declarations, or any other mentioning of types, except in the comments
-}

module Data.HList.VariantP where

import Data.HList.CommonMain
import Data.HList.Label1


-- Declare labels for extensible records -- or, dually, for
-- extensible variants
-- We used the simplest field labels. They are a bit
-- ungainly to use -- but let us avoid overlapping instance extension.
-- See OOHaskell for the use of more advanced labels.

l_nil   = firstLabel
l_cons  = nextLabel l_nil

nil consumer = consumer .!. l_nil

cons a l consumer = (consumer .!. l_cons) a (l consumer)

tl1 c = cons 'a' (cons 'b' (cons 'c' nil)) c
tl2 c = cons 10 (cons 1 (cons 2 (cons 3 nil))) c

{-
  The inferred type of tl1 is

*Variants> :t tl1
tl1 :: (HasField (Label (HSucc HZero)) r (Char -> v -> v),
        HasField (Label HZero) r v) =>
       r -> v

which basically says that tl1 accepts any consumer that has at least
fields l_nil and l_cons with appropriate types. The consumer
may have more fields
-}


-- First polymorphic function: provide a regular list "view" of our list

to_list () =     l_nil  .=. []
             .*. l_cons .=. (:)
             .*. emptyRecord

-- I wish GHC supported unicode in identifiers
tekiyou consumer lst = lst (consumer ())

test1 = tekiyou to_list tl1
-- "abc"
test2 = tekiyou to_list tl2
-- [10,1,2,3]


-- another function, length

lengthL () =  l_nil  .=. 0
          .*. l_cons .=. (\x a -> a + 1)
          .*. emptyRecord

test_lL1 = tekiyou lengthL tl1
-- 3
test_lL2 = tekiyou lengthL tl2
-- 4


-- Now, add extension to our record -- and, dually, extend our variant

l_unit = nextLabel l_cons
l_app  = nextLabel l_unit

unit a c = (c .!. l_unit) a

app l1 l2 c = (c .!. l_app) (l1 c) (l2 c)

tl3 c = cons 1 (unit 2) c
tl4 c = cons 10 (app tl3 tl2) c

sumA () =  l_nil  .=. 0
       .*. l_cons .=. (+)
       .*. l_unit .=. id
       .*. l_app  .=. (+)
       .*. emptyRecord

-- we can apply sum to an original (unextended) list
test_sum1 = tekiyou sumA tl2
-- 16

-- now, we can apply sum to an extended list
test_sum2 = tekiyou sumA tl4
-- 29

-- we can't pass extended lists tl3 and tl4 to a regular lenghtL
-- The error message says that the field l_unit is missing
-- test_lL3 = tekiyou lengthL tl3


-- now we can attempt to extend lengthL, reusing as much of previous
-- functionality as possible

lengthA () =  l_unit .=. const 1
          .*. l_app  .=. (+)
          .*. (lengthL ())

test_lL4 = tekiyou lengthA tl3
-- 2
test_lL5 = tekiyou lengthA tl4
-- 7


-- A few methods to show that our extensible lists indeed act
-- as regular lists (that is, support the regular list API).

-- check if the (extensible list) is null:
el_null l = l (l_nil .=. True .*. l_cons .=. (\_ _ -> False)
               .*. emptyRecord)

el_head l = l (l_nil .=. undefined .*. l_cons .=. (\x _ -> x)
               .*. emptyRecord)

el_tail l = l (    l_nil .=. (\f -> if f then undefined else nil)
               .*. l_cons .=. (\x a f -> if f then a False else
                               cons x (a False))
               .*. emptyRecord) True

test_ell = (el_null tl1, el_head tl1, tekiyou to_list $ el_tail tl1)


-- convert from the regular list
from_list l = foldr cons nil l

test_ft1 = tekiyou to_list $ from_list "abcd"
-- "abcd"


-- Binary methods.
-- Hmm, doing it directly is complicated. OTH, we can use the list view
-- (to_list) and solve the problem this way...
-- Laziness helps...

eqL l1 l2 = tekiyou to_list l1 == tekiyou to_list l2
test_eq1 = eqL tl1 tl1             -- True
test_eq2 = eqL tl1 (cons 'a' nil)  -- False
test_eq3 = eqL tl1 nil             -- False

-- we can extend to_list as usual

to_listA () =  l_unit .=. (:[])
           .*. l_app  .=. (++)
           .*. (to_list ())

-- And we can arbitrary mix old and extended lists
eqA l1 l2 = tekiyou to_listA l1 == tekiyou to_listA l2
test_eq4, test_eq5, test_eq6 :: Bool
test_eq4 = eqA tl1 tl1   -- True
test_eq5 = eqA tl2 tl3   -- False
test_eq6 = eqA tl3 tl4   -- False

