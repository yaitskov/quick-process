{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Process.Th.Predicate.Regex where

import System.Process.Th.Predicate
import System.Process.Th.Prelude
import System.Process.Th.Sbv.Arbitrary
import System.Process.Th.TdfaToSbvRegex as P
import Text.Regex.TDFA ((=~))


data Regex (p :: Symbol) = Regex deriving (Generic)

instance KnownSymbol s => Predicate (Regex s) String where
  validate p x =
    let rx = symbolVal (Proxy @s) in
      if x =~ rx
      then Nothing
      else throwRefineOtherException (typeRep p) $ "Regex " <> show rx <> " mismatches [" <> toText x <> "]"

instance {-# OVERLAPPING #-}
  KnownSymbol p => Arbitrary (Refined (Regex p) String) where

  arbitrary =
    let rx = symbolVal (Proxy @p) in do
      refinErr <$> findStringByRegex (parse rx)

type FsPath = Regex "^([/~]|(~[/]|[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*[/]?)$"
type FsPath2 = Regex "^([/~]|(~[/]|[/])?[a-zA-Z0-9._ -]+([/][a-zA-Z0-9._ -]+)*[/]?)$"
