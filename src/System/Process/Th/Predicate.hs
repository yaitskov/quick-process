{-# LANGUAGE RankNTypes #-}
module System.Process.Th.Predicate where

import Control.Monad.Writer.Strict
import System.Process.Th.Prelude

refinErr :: (Predicate p a, Show a) => a -> Refined p a
refinErr v =
  case refine v of
    Left e -> error $ "Satisfing value [" <> show v <> "] is no valid: " <> show e
    Right vv -> vv

type ArgCollector m = forall v. Data v => v -> m v

class RefinedInArgLocator x where
  locateRefinedInArg :: (MonadIO m, MonadWriter [FilePath] m) => Proxy x -> ArgCollector m

class RefinedOutArgLocator x where
  locateRefinedOutArg :: (MonadIO m, MonadWriter [FilePath] m) => Proxy x -> ArgCollector m
