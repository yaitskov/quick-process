module System.Process.Th.Predicate.InDir where

import Data.Typeable (eqT)
import System.Directory
import System.Process.Th.Predicate
import System.Process.Th.Predicate.InFile ( genFilePathBy )
import System.Process.Th.Prelude
import Text.Regex.TDFA ((=~))
import Type.Reflection ((:~:)(Refl))

data InDir deriving (Data, Show, Eq, Generic)

instance Predicate InDir FilePath where
  validate p x
    | x =~ ("^([.~]?[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*[/]?$" :: String)
    = Nothing
    | otherwise
    = throwRefineOtherException (typeRep p) $ "Bad FilePath " <> toText x <> "]"

instance {-# OVERLAPPING #-} Arbitrary (Refined InDir FilePath) where
  arbitrary =
    genFilePathBy (Proxy @"*") >>= pure . refinErr

findRefinedDirs :: forall m x. (MonadIO m, Data x) => x -> m x
findRefinedDirs x
  | Just Refl <- eqT @x @(Refined InDir FilePath) =
      let fp = unrefine x in do
        liftIO (createDirectoryIfMissing True fp)
        pure x
  | otherwise =
      pure x

instance RefinedInArgLocator (Refined InDir FilePath) where
  locateRefinedInArg _ = findRefinedDirs
