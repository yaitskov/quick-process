{-# LANGUAGE RankNTypes #-}
module System.Process.Th.Predicate where

import Control.Monad.Writer.Strict
import System.Process.Th.Prelude

refinErr :: (Predicate p a, Show a) => a -> Refined p a
refinErr v =
  case refine v of
    Left e -> error $ "Satisfing value [" <> show v <> "] is no valid: " <> show e
    Right vv -> vv

type ArgCollector m =
  (MonadIO m, MonadWriter [FilePath] m) => forall v. Data v => v -> m v

class RefinedInArgLocator x where
  locateRefinedInArg :: Proxy x -> ArgCollector m

class RefinedOutArgLocator x where
  locateRefinedOutArg :: Proxy x -> ArgCollector m
