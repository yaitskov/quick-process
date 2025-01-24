module System.Process.Th.CallSpec where

import Data.Char
import Data.HList
import Language.Haskell.TH as TH
import System.Process.Th.Prelude
import System.Process.Th.CallArgument
import Text.Casing
import Text.Regex

class CallSpec cs where
  programName :: cs -> String
  programArgs :: cs -> [String]

type FoldrConstr l a = (HFoldr (Mapcar (Fun CallArgumentGen (Q a))) [Q a] l [Q a])

genCallArgsRecord :: FoldrConstr l (Maybe VarBangType) => Name -> HList l -> Q Dec
genCallArgsRecord recordName l = do
  fields <- catMaybes <$> sequence (hMapM (Fun fieldExpr :: Fun CallArgumentGen (Q (Maybe VarBangType))) l)
  pure $ DataD [] recordName [] Nothing [RecC recordName fields] []

funD' :: Quote m => Name -> [m Pat] -> m Exp -> m Dec
funD' fname fparams fbody =
  funD fname [clause fparams (normalB fbody) []]

type NonEmptyStr = NonEmpty Char

programNameToHsIdentifier :: String -> Maybe (NonEmpty Char)
programNameToHsIdentifier = nonEmpty . toPascal . fromSnake . underbarred
  where
    underbarred s = subRegex (mkRegex "[^A-Za-z0-9_]") s "_"

genCallSpecInstance :: FoldrConstr l Exp => Name -> String -> HList l  -> Q Dec
genCallSpecInstance recordName progName l =
  instanceD (pure []) [t| CallSpec $(conT recordName) |]
  [ funD' 'programName [ [p|_|] ] [| $(stringE progName) |]
  , funD' 'programArgs []
      [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (Q Exp)) l)) |]
  ]

mkName' :: NonEmptyStr -> Name
mkName' = mkName . toList

newName' :: NonEmptyStr -> Q Name
newName' = newName . toList

-- | gen declaration of CallSpec record with CallSpec instance
genCallSpec ::
  (FoldrConstr l (Maybe VarBangType), FoldrConstr l Exp, Show (HList l)) =>
  String -> HList l -> Q [Dec]
genCallSpec progName l =
  maybe err (g . mkName') (programNameToHsIdentifier progName)
  where
    err = fail $ "Call spec name is bad: " <> show progName <> " " <> show l
    g recName =
      sequence
      [ genCallArgsRecord recName l
      , genCallSpecInstance recName progName l
      ]
