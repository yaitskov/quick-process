module System.Process.Quick.Predicate.InFile where

import System.Process.Quick.Predicate
import System.Process.Quick.Prelude
import System.Process.Quick.TdfaToSbvRegex as P
import System.Process.Quick.Sbv.Arbitrary
import System.Process.Quick.CallArgument (NeList)
import Text.Regex.TDFA ((=~))
import Type.Reflection qualified as R


data InFile (ext :: Symbol) deriving (Data, Show, Eq, Generic)

instance KnownSymbol e => Predicate (InFile e) String where
  validate p x =
    let ext = symbolVal (Proxy @e) in
      if ext == "*"
      then
        if x =~  ("^([.~]?[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*$" :: String)
        then Nothing
        else throwRefineOtherException (typeRep p) $ "Bad FilePath " <> toText x <> "]"
      else
        if x =~ ("^([.~]?[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*[.]" <> ext <> "$")
        then Nothing
        else throwRefineOtherException (typeRep p) $ "Bad FilePath " <> toText x <> "]"

instance Predicate (InFile e) String => Predicate (InFile e) [String] where
  validate p = listToMaybe . mapMaybe (validate p)
instance Predicate (InFile e) String => Predicate (InFile e) (NeList String) where
  validate p = listToMaybe . mapMaybe (validate p) . toList

genFilePathBy :: forall e. KnownSymbol e => Proxy e -> Gen FilePath
genFilePathBy _ =
  let ext = symbolVal (Proxy @e) in
    findStringByRegex
      (parse $ if ext == "*"
        then "^[^/\x0000-\x001F]+([.][a-z]{1,4})?$"
        else "^[^/\x0000-\x001F]+[.]" <> ext <> "$")

instance {-# OVERLAPPING #-}
  KnownSymbol e => Arbitrary (Refined (InFile e) FilePath) where
  arbitrary =
    genFilePathBy (Proxy @e) >>= pure . refinErr

instance {-# OVERLAPPING #-}
  KnownSymbol e => Arbitrary (Refined (InFile e) [FilePath]) where
  arbitrary =
    sized $ \n -> refinErr <$> mapM (\_ -> genFilePathBy $ Proxy @e) (take n [1::Int ..])

instance {-# OVERLAPPING #-}
  KnownSymbol e => Arbitrary (Refined (InFile e) (NeList FilePath)) where
  arbitrary =
    sized $ \n -> refinErr <$> mapM (\_ -> genFilePathBy $ Proxy @e) (0 :| take n [1::Int ..])

data OutFile (ext :: Symbol)

instance KnownSymbol e => Predicate (OutFile e) String where
  validate p x =
    let ext = symbolVal (Proxy @e) in
      if ext == "*"
      then
        if x =~  ("^([.~]?[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*$" :: String)
        then Nothing
        else throwRefineOtherException (typeRep p) $ "Bad FilePath " <> toText x <> "]"
      else
        if x =~ ("^([.~]?[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*[.]" <> ext <> "$")
        then Nothing
        else throwRefineOtherException (typeRep p) $ "Bad FilePath " <> toText x <> "]"

instance {-# OVERLAPPING #-}
  KnownSymbol e => Arbitrary (Refined (OutFile e) FilePath) where
  arbitrary =
    let ext = symbolVal (Proxy @e) in do
      sv <- findStringByRegex
        (parse $ if ext == "*"
          then "^[^/\x0000-\x001F]+([.][a-z]{1,4})?$"
          else "^[^/\x0000-\x001F]+[.]" <> ext <> "$")
      case refine sv of
        Left e -> error $ "Satisfing value [" <> show sv <> "] is no valid: " <> show e
        Right vv -> pure vv

findRefinedStrings :: forall v p m x.
  ( Typeable p
  , MonadWriter [FilePath] m
  , MonadIO m
  , Typeable x
  , Typeable v
  , Data x
  ) => Proxy p -> (v -> [String]) -> x -> m x
findRefinedStrings _ f x
  | _rRefined `R.App` rif@(R.TypeRep @tif) `R.App` _rString <- R.TypeRep @x
  , R.TypeRep <- R.typeRepKind rif
  , Just Refl <- eqT @x @(Refined tif v)
  , rInFile `R.App` _rExt <- R.TypeRep @tif
  , Just R.HRefl <- R.eqTypeRep  rInFile (R.typeRep :: R.TypeRep p)
  = let fp = unrefine x in tell (f fp) >> pure x
  | otherwise = pure x

instance RefinedInArgLocator (Refined (InFile e) FilePath) where
  locateRefinedInArg _ = findRefinedStrings (Proxy @InFile) pure
instance RefinedInArgLocator (Refined (InFile e) (NeList FilePath)) where
  locateRefinedInArg _ = findRefinedStrings @(NeList FilePath) (Proxy @InFile) toList
instance RefinedInArgLocator (Refined (InFile e) [FilePath]) where
  locateRefinedInArg _ = findRefinedStrings (Proxy @InFile) id
instance RefinedOutArgLocator (Refined (OutFile e) FilePath) where
  locateRefinedOutArg _ = findRefinedStrings (Proxy @OutFile) pure
