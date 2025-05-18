module System.Process.Th.TdfaToSbvRegex (parse, match) where

import Data.SBV.RegExp
import System.Process.Th.Prelude
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex

parse :: String -> RegExp
parse rxp =
  case parseRegex $ adaptAnchors rxp of
    Right (p, _) -> tdfa2SbvRegex p
    Left e -> error $ "Failed to parse pattern " <> show rxp <> " as TDFA due: " <> show e

adaptAnchors :: String -> String
adaptAnchors [] = ".*"
adaptAnchors rx = tailAnchor $ headAnchor rx
  where
    headAnchor x =  if "^" `isPrefixOf` x then x else ".*" <> x
    tailAnchor x =  if "$" `isSuffixOf` x then x else x <> ".*"

tdfa2SbvRegex :: Pattern -> RegExp
tdfa2SbvRegex = \case
  PEmpty -> Literal ""
  PGroup _ p -> tdfa2SbvRegex p -- ??
  POr ps -> Union $ fmap tdfa2SbvRegex ps
  PConcat ps -> Conc $ fmap tdfa2SbvRegex ps
  PQuest p -> Opt $ tdfa2SbvRegex p
  PPlus p -> KPlus $ tdfa2SbvRegex p
  PStar _ p -> KStar $ tdfa2SbvRegex p
  PBound n Nothing p -> Power n $ tdfa2SbvRegex p
  PBound n (Just m) p -> Loop n m $ tdfa2SbvRegex p
  PCarat _ -> Literal "" -- store in state monad as global flag
  PDollar _ -> Literal "" -- store in state monad as global flag
  PDot _ -> AllChar
  PAny _ ps -> patternSetToRegex ps
  PAnyNot _ ps -> Inter AllChar . Comp $ patternSetToRegex ps
  PEscape _ c -> Literal [c]
  PChar _ c -> Literal [c]
  PNonCapture p -> tdfa2SbvRegex p -- ??
  PNonEmpty p -> tdfa2SbvRegex p -- ??

patternSetToRegex :: PatternSet -> RegExp
patternSetToRegex = \case
  PatternSet ps Nothing Nothing Nothing ->
    Union (fromMaybe [] (charSetToRegex <$> ps))
  PatternSet _ a b c ->
    error $ "Not supported:" <> show a <> "; " <> show b <> "; " <> show c

charSetToRegex :: Set Char -> [RegExp]
charSetToRegex = fmap (Literal . (:[])) . toList
