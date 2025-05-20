{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
module System.Process.Th.Predicate.InputFile where

import System.Process.Th.Prelude
import System.Process.Th.TdfaToSbvRegex as P
import System.Process.Th.CallEffect
import System.Process.Th.Sbv.Arbitrary
import Text.Regex.TDFA ((=~))


data InFile (ext :: Symbol)

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
