{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}

module System.Process.Th.Predicate.InputFile where

import Control.Monad.Writer.Strict
import System.Process.Th.Prelude
import System.Process.Th.TdfaToSbvRegex as P
import System.Process.Th.CallEffect
import System.Process.Th.Sbv.Arbitrary
import Text.Regex.TDFA ((=~))
import Type.Reflection qualified as R
import Type.Reflection ((:~:)(Refl))
import Data.Typeable (eqT)

data InFile (ext :: Symbol) deriving (Typeable, Data, Show, Eq, Generic)

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


instance {-# OVERLAPPING #-}
  KnownSymbol e => Arbitrary (Refined (InFile e) FilePath) where
  arbitrary =
    let ext = symbolVal (Proxy @e) in do
      sv <- findStringByRegex
        (parse $ if ext == "*"
          then "^[^/\x0000-\x001F]+([.][a-z]{1,4})?$"
          else "^[^/\x0000-\x001F]+[.]" <> ext <> "$")
      case refine sv of
        Left e -> error $ "Satisfing value [" <> show sv <> "] is no valid: " <> show e
        Right vv -> pure vv

data Ts (x :: Maybe TimeReference) (y :: k)
data UxFsPerm = UxFsPerm Natural
data Perm (x :: UxFsPerm) (y :: k)

type Foo = Ts (Just LaunchTime) (InFile "xml")

type Bar = Perm ('UxFsPerm 0) (InFile "csv")
type FooBar = Ts Nothing (Perm ('UxFsPerm 0777) (InFile "json"))


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

findRefinedStrings :: forall p m x.
  ( Typeable p
  , MonadWriter [FilePath] m
  , MonadIO m
  , Typeable x
  , Data x
  ) => Proxy p -> x -> m x
findRefinedStrings _ x
  | _rRefined `R.App` rif@(R.TypeRep @tif) `R.App` _rString <- R.TypeRep @x
  , R.TypeRep <- R.typeRepKind rif
  , Just Refl <- eqT @x @(Refined tif String)
  , rInFile `R.App` _rExt <- R.TypeRep @tif
  , Just R.HRefl <- R.eqTypeRep  rInFile (R.typeRep :: R.TypeRep p)
  = let fp = unrefine x in tell [fp] >> pure x
  | otherwise = pure x

findInFile :: forall m x. (MonadWriter [FilePath] m, MonadIO m, Data x) => x -> m x
findInFile = findRefinedStrings (Proxy @InFile)

findOutFile :: forall m x. (MonadWriter [FilePath] m, MonadIO m, Data x) => x -> m x
findOutFile = findRefinedStrings (Proxy @OutFile)
