module System.Process.Th.CallSpec where

import Data.HList
import Language.Haskell.TH as TH
import System.Process.Th.Prelude
import System.Process.Th.CallArgument
import Text.Regex

class CallSpec cs where
  programName :: cs -> String
  programArgs :: cs -> [String]

type FoldrConstr l a = (HFoldr (Mapcar (Fun CallArgumentGen (Q a))) [Q a] l [Q a])

genCallArgsRecord :: FoldrConstr l (Maybe VarBangType) => Name -> HList l -> Q Dec
genCallArgsRecord recordName l = do
  fields <- catMaybes <$> sequence (hMapM (Fun fieldExpr :: Fun CallArgumentGen (Q (Maybe VarBangType))) l)
  pure $ DataD [] recordName [] Nothing [RecC recordName fields] []

-- | generate function :: CallSpec -> [String]
genProgArgs :: FoldrConstr l Exp => String -> Name -> HList l -> Q [Dec]
genProgArgs fName recTypeName l = do
  fBody <- [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (Q Exp)) l)) |]
  sig <- sigD fName' [t| $(conT recTypeName) -> [ String ] |]
  pure [sig, FunD fName' [Clause [] (NormalB fBody) []]]
  where
    fName' = mkName fName

programNameToHsIdentifier :: String -> String
programNameToHsIdentifier pn = "CS_" <> underbarrred
  where
    underbarrred = subRegex (mkRegex "[^A-Za-z0-9_]") pn "_"

-- | gen declaration of CallSpec record with CallSpec instance
genProgArgsRender ::
  (FoldrConstr l (Maybe VarBangType), FoldrConstr l Exp) =>
  String -> HList l -> Q [Dec]
genProgArgsRender progName l = do
  recDec <- genCallArgsRecord recName l
  progArgs <- genProgArgs progArgsFunName recName l
  pure $ recDec : progArgs
  where
    progName' = programNameToHsIdentifier progName
    progArgsFunName = "render" <>  progName' <> "CallArgs"
    recName = mkName progName'
