
{- | Description: keyword arguments

The public interface is exposed in <Data-HList-CommonMain.html#t:Kw CommonMain#Kw>

-}
module Data.HList.Keyword (

  -- * main
  Kw(..),
  IsKeyFN,

  recToKW,

  -- ** another label type
  K(..),

  -- * types for user error
  ErrReqdArgNotFound,
  ErrUnexpectedKW,


  -- * demo
  -- ** setup data types
  -- $setup
  -- $ex2

  -- * Implementation details
  -- $imploutline
  KWApply(..),
  KWApply'(..),
  Arg(..),



  -- ** producing lists from a function's arguments
  reflect_fk,
  ReflectFK,
  ReflectFK',


  -- ** collecting arguments
  KW(..),
  KW'(..),
  KWAcc(..),

  -- ** merging default with supplied arguments
  KWMerge(..),
  KWMerge'(..),
  KWMerge''(..),

  HDelete, HDelete',


  -- * original introduction
  -- $originalIntro


  -- * todo
  -- $todo

  -- * internal for type signature prettiness
  TaggedToKW,
  ) where

import GHC.TypeLits
import Data.HList.FakePrelude
import Data.HList.TypeEqO
import Data.HList.HListPrelude
import Data.HList.HList
import Data.HList.Record

{- $setup

 >>> :set -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses
 >>> :set -XScopedTypeVariables -XOverlappingInstances -XTypeFamilies
 >>> :set -fcontext-stack=100

We will be using an example inspired by a graphics toolkit -- the area
which really benefits from keyword arguments. We first define our
labels and useful datatypes

 >>> data Color = Color
 >>> data Size  = Size
 >>> data Origin  = Origin
 >>> data RaisedBorder = RaisedBorder


The number of arguments each keyword must be specified by an 'IsKeyFN'
instance.

 >>> instance IsKeyFN (Color->a->b)  True
 >>> instance IsKeyFN (Size->a->b)   True
 >>> instance (a ~ (Int,Int)) => IsKeyFN (Origin->a->b) True
 >>> instance IsKeyFN (RaisedBorder->a->b) True

Note that if a keyword is always followed by a certain type, that
can be specified above using an instance like the one for Origin.

 >>> data CommonColor = Red | Green | Blue deriving Show
 >>> data RGBColor = RGBColor Int Int Int deriving Show

and two functions:

 >>> :{
 let make_square Size n Origin (x0,y0) Color (color::CommonColor) =
        unwords ["Square:", show (n :: Int), "at", show (x0,y0), show color] ++ "\n"
 :}

 >>> :{
 let make_rect Size (nx,ny) Origin (x0,y0) Color (color::RGBColor)
         RaisedBorder border =
        unwords ["Rectangle:", show (nx,ny), "at", show (x0,y0),
             show color, if border then "raised border" else ""] ++ "\n"
 :}

-}

{- $ex2

We are not interested in really drawing squares and rectangles
here. Therefore, make_square and make_rect return a String, which we
can regard as a ``command'' to be passed to a low-level graphics
library. The functions make_square and make_rect are genuine functions
and can be used as such. They are not keyword argument functions, yet,
but they set the stage. These functions can be considered an
`interface' for the keyword argument functions. We should note that
the functions are polymorphic: for example, `Size' can be any
showable. We must also emphasize the re-use of the labels: The Color
of a square is the value of the enumerated type CommonColor. OTH, the
color of the rectangle is given as an RGB triple. The sizes of the
square and of the rectangle are specified differently too, the same
label notwithstanding.

Once the user wrote the functions such as make_square and make_rect,
he can _automatically_ convert them to their keyword
alternatives. This transformation is done by a function 'kw'. The user
should pass the positional-argument function (`labeled' as above),
and an HList of default values for some of the labels. The latter may
be HNil if all keyword arguments are required.

The first example (no defaults)

 >>> kw (make_square .*. HNil) Size (1::Int) Origin (0,10) Color Red   :: String
 "Square: 1 at (0,10) Red\n"

we can permute the arguments at wish

 >>> kw (make_square .*. HNil) Color Red Size (1::Int) Origin (0,10)   :: String
 "Square: 1 at (0,10) Red\n"

we can also assign a name to a keyword function, or partially apply it:

 >>> :{
 case kw (make_square .*. HNil) Color Red of
    f -> "here: " ++ f Origin (0,10) Size (1::Int)
:}
"here: Square: 1 at (0,10) Red\n"

Note that it is necessary to use a monomorphic pattern binding here (lambda or
case). One way to get around this is to pass @f@ instead of @kw f@ around:

>>> :{
 let f = hEnd $ hBuild make_square Color Red
 in "here: " ++ kw f Origin (0,10) Size (1::Int)
:}
"here: Square: 1 at (0,10) Red\n"

The following is a more interesting example, with the
defaults:

 >>> :{
let addDef f = f .*. Origin .*. (0,10) .*.
             RaisedBorder .*. True .*.
             HNil
    in kw (addDef make_square) Size (1::Int) Color Red ++
       kw (addDef make_rect)   Color (RGBColor 0 10 255)
                               Size (1.0::Float, 2.0::Float)
:}
"Square: 1 at (0,10) Red\nRectangle: (1.0,2.0) at (0,10) RGBColor 0 10 255 raised border\n"

The argument RaisedBorder is not given, and so the default value is
used. Of course, we can override the default:

 >>> :{
let addDef f =  f .*. Origin .*. (0,10) .*.
                    RaisedBorder .*. True .*.
                    HNil
 in case kw (addDef make_square) Color of
     sq -> case kw (addDef make_rect)  of
      re ->
         sq Red Size (1::Int) ++
         re Color (RGBColor 0 10 255)
             RaisedBorder False
             Size (1.0::Float, 2.0::Float)
:}
"Square: 1 at (0,10) Red\nRectangle: (1.0,2.0) at (0,10) RGBColor 0 10 255 \n"

We have reshuffled a few arguments, just for fun. As you can see, the
function `kw make_rect defaults' is polyvariadic indeed.  We chose to
partially apply 'Color' to the function `kw make_square defaults' --
so that the function `sq' is positional in its first argument, and
keyword in the rest.

If we omit a required argument, we get a type error:

> ] testse1 = let f x = kw make_square HNil Color Red x
> ]         in "here: " ++ f Origin (0,10)
>
>   Couldn't match `ErrReqdArgNotFound Size' against `[Char]'
>       Expected type: ErrReqdArgNotFound Size
>       Inferred type: [Char] ...

The error message seems reasonably clear. Likewise we get an error
message if we pass to a keyword function an argument it does not expect:

> ] testse2 = let f x = kw make_square HNil Color Red x
> ]         in "here: " ++ f Origin (0,10) Size (1::Int)
> ]                       RaisedBorder False
>
>   No instances for (Fail (ErrUnexpectedKW RaisedBorder),
>             KWApply [Char] (HCons RaisedBorder (:*: Bool HNil)) [Char])
>       arising from use of `f' at ...
>     In the second argument of `(++)', namely
>   `f Origin (0,10) Size (1 :: Int) RaisedBorder False'


The function 'kw' receives the appropriately labeled function (such
as make_square) and the HList with default values. The function 'kw'
is polymorphic; the overloading is resolved based on the type of the
user function *and* on the type of its continuation. The continuation
indicates if a keyword argument is forthcoming, or not. In the latter
case, 'kw' checks to see if the supplied defaults can provide the
values of the still missing arguments. We see therefore that a
function type is more than it may appear -- the type of a function is
truly a heterogeneous, type level collection! The function 'kw'
traverses that collection, thus performing a limited form of
reflection on Haskell functions.

-}


{- $imploutline

One of the key tools of the implementation is 'kwapply', which applies
a function to a polymorphic collection of that function's arguments.
The order of the arguments in the collection is irrelevant. The
contraption kwapply can handle polymorphic functions with arbitrary
number of labeled arguments.

For example, if we define

> f1 Size n = show n
> f2 Size n Color m = unwords ["size:", show n, "color:", show m]
> f3 Origin x Color m Size n =
>     unwords ["origin:", show x, "size:", show n, "color:",show m]

then we can run

> katest1  = kwapply f1 (Size .*. () .*. HNil)
> katest11 = kwapply f1 (Size .*. "Large" .*. HNil)
>
> katest2  = kwapply f2 (Size .*. (1::Int) .*. Color .*. Red .*. HNil)
> katest21 = kwapply f2 (Color .*. Red .*. Size .*. (1::Int) .*.  HNil)
>
> katest3  = kwapply f3 (Size .*. (1::Int) .*. Origin .*. (2.0::Float) .*.
>                  Color .*. Red .*. HNil)

-}


-- | Another key contraption is

reflect_fk:: (ReflectFK fn kws) => fn -> Arg kws '[]
reflect_fk _ = Arg HNil

{- ^

that reflects on a user-supplied function. It converts the *type* of a
user function to a collection of keywords required by that
function. This and the previous contraptions may be used to define an
`extended' version of some user function that takes more arguments --
without the need to enumerate all arguments of the original
function. We thus infringe on the area of object and module systems.

The rest of the implementation is just to convert `kw fn defaults'
into the application of kwapply.

-}


-- * The rest of the implementation
{- $impl

We should note that all implementation is written in the
continuation-passing style (CPS) -- at the term level and especially
at the _typeclass level_. One of the reasons is to avoid relying on
overlapping instances: we compare types with a predicate `TypeEq x y
hbool', obtain the type-level boolean, and dispatch to two
non-overlapping instances of an auxiliary, continuation class. One
instance handles HTrue, and the other the HFalse alternative. Please
see the HList paper for more discussion of this technique.

The other, equally important reason for the thorough CPS of the
typeclasses is to control the order of evaluation of constraints and
their functional dependencies. The sole reason is to produce
informative error messages. The order of constraints is irrelevant
when all the constraints are satisfied. However, if the user omitted a
required keyword, many of the constraints below will fail. If a
'wrong' constraint fails first, we get a totally off-the-wall error
message that gives us no clue whatsoever about the problem. By tightly
constraining the order via CPS, we are able to force the typechecker
to give informative error messages.

-}



-- * Errors

data ErrReqdArgNotFound x
data ErrUnexpectedKW x


instance IsKeyFN (Label (s :: Symbol) -> a -> b) True
{- ^ labels that impose no restriction on the type of the (single) argument
 which follows

 >>> let testF (_ :: Label "a") (a :: Int) () = a+1
 >>> kw (hBuild testF) (Label :: Label "a") 5 ()
 6

-}

{- | The purpose of this instance is to be able to use the same Symbol
 (type-level string) at different types. If they are supposed to be the same,
 then use 'Label' instead of 'K'

 >>> let kA = K :: forall t. K "a" t
 >>> let testF (K :: K "a" Int) a1 (K :: K "a" Integer) a2 () = a1-fromIntegral a2

 therefore the following options works:

 >>> kw (hBuild testF) kA (5 :: Int) kA (3 :: Integer) ()
 2

 >>> kw (hBuild testF) (K :: K "a" Integer) 3 (K :: K "a" Int) 5 ()
 2

 But you cannot leave off all @Int@ or @Integer@ annotations.

-}
instance (r ~ (c -> b)) => IsKeyFN ( (K s c) -> r) True

data K s (c :: *) = K


-- * The implementation of KWApply

class KWApply f arg_values r where
    kwapply:: f -> HList arg_values -> r

instance (r ~ r') => KWApply r '[] r' where
    kwapply f _ = f

instance (HEq kw kw' flag,
      KWApply' flag (kw ->a->f') (kw' ': a' ': tail) r)
    => KWApply (kw ->a->f') (kw' ': a' ': tail) r where
    kwapply = kwapply' (Proxy :: Proxy flag)

class KWApply' flag f arg_values r  where
    kwapply':: Proxy flag -> f -> HList arg_values -> r

instance  (v' ~ v, KWApply f' tail r)
    => KWApply' True (kw->v->f') (kw ': v' ': tail) r where
    kwapply' _ f (HCons kw_ (HCons v' tl)) =
                   kwapply (f kw_ v') tl

-- | Rotate the arg list ...
instance  (HAppendListR tail '[kw , v] ~ l',
           HAppendList tail '[kw, v],
       KWApply f l' r)
    => KWApply' False f (kw ': v ': tail) r where
    kwapply' _ f (HCons kw_ (HCons v tl)) =
      kwapply f (hAppend tl (kw_ .*. v .*. HNil))

{- |

The datatype Arg below is to maintain the state of keyword
accumulation: which keywords we need, and which keyword and values we
have already got.
arg_types is the phantom HList of keywords that are yet to be satisfied.
arg_values is the @HList (kw .*. kw_value .*. etc)@
of already found keywords and their values.
-}

newtype Arg arg_types arg_values = Arg (HList arg_values)
deriving instance Show (HList vals) => Show (Arg tys vals)

{- | Reflection on a function:
Given a function, return the type list of its keywords

>>> :t reflect_fk (undefined::Size->Int->Color->CommonColor->String)
reflect_fk (undefined::Size->Int->Color->CommonColor->String)
  :: Arg '[Size, Color] '[]

>>> :t reflect_fk (undefined::Size->Int->()->Int)
reflect_fk (undefined::Size->Int->()->Int) :: Arg '[Size] '[]


-}

class ReflectFK f (kws :: [*])
instance (IsKeyFN f flag, ReflectFK' flag f kws) => ReflectFK f kws
class ReflectFK' (flag :: Bool) f kws
instance (kkws ~ (kw ': kws), ReflectFK rest kws) => ReflectFK' True (kw->a->rest) kkws
instance ('[] ~ nil) => ReflectFK' False f nil


-- | The main class: collect and apply the keyword arguments

class KW f arg_desc arg_def r where
    kwdo :: f -> arg_desc -> HList arg_def -> r

instance (IsKeyFN r rflag,
        KW' rflag f arg_desc arg_def r)
    => KW f arg_desc arg_def r where
    kwdo = kw' (Proxy ::Proxy rflag)

class KW' rflag f arg_desc arg_def r where
    kw' :: Proxy rflag -> f -> arg_desc -> HList arg_def -> r

{- |
If the continuation r does not promise any more keyword
arguments, apply the defaults -}

instance KWMerge arg_needed arg_values arg_def f r
    => KW' False f (Arg arg_needed arg_values) arg_def r where
    kw' _ f args_given arg_def = kwmerge args_given arg_def f

{- | Otherwise, collect the supplied keyword and its value, and recurse for
more: -}

instance (KWAcc arg_desc kw a f arg_def r, (kw->a->r) ~ kwar)
    => KW' True f arg_desc arg_def kwar where
    kw' _ f arg_desc arg_def kw_ a = kwaccum arg_desc kw_ a f arg_def


{- | Add the needed arguments from arg_def to arg_values and continue
with kwapply.

That is, we try to satisfy the missing arguments from the defaults.
It will be a type error if some required arguments are missing -}

class KWMerge arg_needed arg_values arg_def f r where
    kwmerge:: Arg arg_needed arg_values -> HList arg_def -> f -> r

instance KWApply f arg_values r
    => KWMerge '[] arg_values arg_def f r where
    kwmerge (Arg arg_values) _ f = kwapply f arg_values

instance KWMerge' kw arg_def atail arg_values arg_def f r
    => KWMerge (kw ': atail) arg_values arg_def f r where
    kwmerge (Arg arg_values) arg_def =
      kwmerge' (undefined :: kw) arg_def
             ((Arg arg_values)::Arg atail arg_values) arg_def

class KWMerge' kw list atail arg_values arg_def f r where
    kwmerge':: kw -> HList list -> (Arg atail arg_values) -> HList arg_def -> f -> r

instance (Fail (ErrReqdArgNotFound kw), nff ~ (ErrReqdArgNotFound kw))
    => KWMerge' kw '[] atail arg_values arg_def f
                nff where
    kwmerge' = undefined
instance (HEq kw kw' flag,
      KWMerge'' flag kw (kw' ': etc) atail arg_values arg_def f r)
    => KWMerge' kw (kw' ': etc) atail arg_values arg_def f r where
    kwmerge' = kwmerge'' (Proxy :: Proxy flag)

class KWMerge'' (flag :: Bool) kw (list :: [*]) atail arg_values arg_def f r
     where
    kwmerge'':: Proxy flag -> kw -> HList list
        -> Arg atail arg_values -> HList arg_def
        -> f -> r
instance KWMerge atail (kw ': v ': arg_values) arg_def f r
    => KWMerge'' True kw (kw ': v ': tail)
                 atail arg_values arg_def f r where
    kwmerge'' _ _ (HCons kw_ (HCons v _)) (Arg arg_values) =
      kwmerge ((Arg (kw_ .*. v .*. arg_values))::
         (Arg atail (kw ': v ': arg_values)))
instance KWMerge' kw tail atail arg_values arg_def f r
    => KWMerge'' False kw (kw' ': v' ': tail)
                 atail arg_values arg_def f r where
    kwmerge'' _ kw_ (HCons _ (HCons _ tl)) = kwmerge' kw_ tl

-- | Add the real argument to the Arg structure, and continue

class KWAcc arg_desc kw a f arg_def r where
    kwaccum:: arg_desc -> kw -> a -> f -> HList arg_def -> r


instance (HDelete kw arg_types arg_types',
      KW f (Arg arg_types' (kw ': a ': arg_values)) arg_def r)
    => KWAcc (Arg arg_types arg_values) kw a f arg_def r  where
    kwaccum (Arg arg_values) kw_ a f =
      kwdo f (Arg (kw_ .*. a .*. arg_values)::
        Arg arg_types' (kw ': a ': arg_values))


-- | Delete e from l to yield l' The element e must occur in l

class HDelete e (l :: [k]) (l' :: [k])
instance (Fail (ErrUnexpectedKW e), r ~ '[]) => HDelete e '[] r
instance (HEq e e' flag, HDelete' flag e (e' ': tail) l')
    => HDelete e (e' ': tail) l'
class HDelete' (flag :: Bool) e l l'
instance (tail' ~ tail) => HDelete' True e (e ': tail) tail'
instance (HDelete e tail tail', e'tail ~ (e' ': tail'))
    => HDelete' False e (e' ': tail) e'tail


{- |

@kw@ takes a 'HList' whose first element is a function, and the rest
of the elements are default values.
A useful trick is to have a final argument @()@ which is not
eaten up by a label (A only takes 1 argument). That way when you supply
the () it knows there are no more arguments (?).

>>> data A = A
>>> instance IsKeyFN (A -> a -> b) True
>>> let f A a () = a + 1
>>> let f' = f .*. A .*. 1 .*. HNil

>>> kw f' A 0 ()
1

>>> kw f' ()
2

-}
class Kw (fn :: *) (arg_def :: [*]) r where
    kw :: HList (fn ': arg_def) -> r

instance
    (KW' rflag fn akws arg_def r,
     akws ~ (Arg (kws :: [*]) '[]),
     ReflectFK' flag fn kws, IsKeyFN r rflag,
     IsKeyFN fn (flag::Bool)) => Kw fn arg_def r
   where
    kw (HCons f arg_def) = kwdo f rfk arg_def :: r
        where rfk = reflect_fk f :: akws

data TaggedToKW = TaggedToKW
instance (x ~ Tagged l v, y ~ HList '[Label l, v]) =>
        ApplyAB TaggedToKW x y where
    applyAB _ (Tagged v) = hBuild Label v


{- | convert a 'Record' into a list that can supply
default arguments for 'kw'

A bit of setup:

>>> :set -XQuasiQuotes
>>> import Data.HList.RecordPuns
>>> let f (_ :: Label "a") a (_ :: Label "b") b () = a `div` b


>>> let a = 2; b = 1; f' = f .*. recToKW [pun| a b |]
>>> kw f' ()
2

>>> kw f' (Label :: Label "a") 10 ()
10


-}
recToKW :: forall a b. (HMapCxt HList TaggedToKW a b, HConcat b) =>
     Record a -> HList (HConcatR b)
recToKW (Record r) = hConcat (hMap TaggedToKW r :: HList b)

{- $originalIntro

> From oleg-at-okmij.org Fri Aug 13 14:58:35 2004
> To: haskell@haskell.org
> Subject: Keyword arguments
> From: oleg-at-pobox.com
> Message-ID: <20040813215834.F1FF3AB7E@Adric.metnet.navy.mil>
> Date: Fri, 13 Aug 2004 14:58:34 -0700 (PDT)
> Status: OR


We show the Haskell implementation of keyword arguments, which goes
well beyond records (e.g., in permitting the re-use of
labels). Keyword arguments indeed look just like regular, positional
arguments. However, keyword arguments may appear in any
order. Furthermore, one may associate defaults with some keywords; the
corresponding arguments may then be omitted. It is a type error to
omit a required keyword argument. The latter property is in stark
contrast with the conventional way of emulating keyword arguments via
records. Also in marked contrast with records, keyword labels may be
reused throughout the code with no restriction; the same label may be
associated with arguments of different types in different
functions. Labels of Haskell records may not be re-used.  Our solution
is essentially equivalent to keyword arguments of DSSSL Scheme or
labels of OCaml.

Keyword argument functions are naturally polyvariadic: Haskell does
support varargs! Keyword argument functions may be polymorphic. As
usual, functions with keyword arguments may be partially applied. On
the downside, sometimes one has to specify the type of the return
value of the function (if the keyword argument function has no
signature -- the latter is the norm, see below) -- provided that the
compiler cannot figure the return type out on its own. This is usually
only the case when we use keyword functions at the top level (GHCi
prompt).

Our solution requires no special extensions to Haskell and works with
the existing Haskell compilers; it is tested on GHC 6.0.1. The
overlapping instances extension is not necessary (albeit it is
convenient).

The gist of our implementation is the realization that the type of a
function is a polymorphic collection of its argument types -- a
collection that we can traverse. This message thus illustrates a
limited form of the reflection on a function.


Our implementation is a trivial extension of the strongly-typed
polymorphic open records described in
    <http://homepages.cwi.nl/~ralf/HList/>

In fact, the implementation relies on the HList library.  To run the
code (which this message is), one needs to download the HList library
from the above site.

The HList paper discusses the issue of labels in some detail. The
paper gives three different representations. One of them needs no
overlapping instances and is very portable. In this message, we chose
a representation that relies on generic type equality and therefore
needs overlapping instances as implemented in GHC. Again, this is
merely an outcome of our non-deterministic choice. It should be
emphasized that other choices are possible, which do not depend on
overlapping instances at all. Please see the HList paper for details.

-}


{- $todo

[@better instances for Symbol@]

There isn't a pair @(K2 \"Origin\" (Int, Int))@ @(K \"hi\")@ that behaves just like Origin below.
something is possible between constraintkinds. See 'Data.HList.FakePrelude.Fun'

> instance (a ~ (Int,Int)) => IsKeyFN (Origin->a->b) True

[@wildcard/catchall@]

like in R. This would be a special keyword for keyword args that didn't match.
They would be put in a HList/Record argument like @...@

[@investigate first-classness of varargs@]
for whatever reason you can't have  @f = kw fn blah@ and then pass more arguments
on to fn. This is bad. It used to work (in the ghc6.0 days and probably up to
6.12). Some convenience functions/operators should be added which do the same
thing as:

> fn `hAppendList` hBuild a b c d e


-}
