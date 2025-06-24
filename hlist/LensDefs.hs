{-# LANGUAGE CPP #-}
-- | parts of lens that would be imported if we depended on it
module LensDefs
  (module LensDefs,
   module Control.Applicative,
   Choice,
   Profunctor,
   Coercible)
   where

import Data.Profunctor
import Data.Profunctor.Unsafe
import Control.Applicative
import Control.Monad.Identity

import Unsafe.Coerce
#if __GLASGOW_HASKELL__ > 707
import GHC.Exts(Coercible)
#else
import GHC.Exts(Constraint)
-- | for ghc-7.6 we don't have coercible
type Coercible a b = (() :: Constraint)
#endif


type Equality' s a = forall p (f :: * -> *). a `p` f a -> s `p` f s

{- | if we write @f' = simple . f@, then the inferred type is

> f' :: (s ~ t, _) => Lens s t a b

which normally will let ghc figure out (a~b). However with the
types that come up in HList this can only be figure out with
concrete types, so we use isSimple instead which also specifies
(a~b).

-}
isSimple :: optic ~ (p a (f a) -> p s (f s)) => optic -> optic
isSimple = id
-- alternatively: isSimple x = simple . x . simple

simple :: Equality' a a
simple = id

-- Used by doctests (which should probably just import Control.Lens...)
infixl 1 &
x & f = f x

infixr 4 %~
l %~ f = \t -> runIdentity $ l (rmap Identity f) t

iso :: (Profunctor p, Functor f)
    => (s -> a) -> (b -> t)
    -> p a (f b) -> p s (f t)
iso sa bt = dimap sa (fmap bt)

-- | iso, except assumes that the functions supplied could
-- be 'Data.Coerce.coerce'
isoNewtype :: (Profunctor p, Functor f,
               Coercible b t, -- Coercible (f b) (f t) -- is really needed but that complicates types later on (since f is forall'd)
               Coercible a s)
    => (s -> a) -> (b -> t)
    -> p a (f b) -> p s (f t)
isoNewtype sa _bt x = coerceBT x .# sa
  where coerceBT :: p a (f b) -> p a (f t)
        coerceBT = unsafeCoerce

prism :: (b -> t) -> (s -> Either t a)
    -> (forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t))
prism bt seta = dimap seta (either pure (fmap bt)) . right'

prism' :: (a -> s) -> (s -> Maybe a)
    -> (forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s))
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
