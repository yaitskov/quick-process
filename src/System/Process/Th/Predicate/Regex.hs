{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Process.Th.Predicate.Regex where

import System.Process.Th.Prelude
import System.Process.Th.TdfaToSbvRegex as P
import Text.Regex.TDFA ((=~))
-- import Data.SBV (Satisfiable, SymVal, Modelable (..), SString, sat, (.==), (.&&), literal)
-- import Data.SBV.String qualified as S

import System.Process.Th.Sbv.Arbitrary


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
      sv <- findStringByRegex (parse rx)
      case refine sv of
        Left e -> error $ "Satisfing value [" <> show sv <> "] is no valid: " <> show e
        Right vv -> pure vv

type FsPath = Regex "^([/~]|(~[/]|[/])?[^/\x0000-\x001F]+([/][^/\x0000-\x001F]+)*[/]?)$"
