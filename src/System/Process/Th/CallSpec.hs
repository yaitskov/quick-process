{-# LANGUAGE TemplateHaskellQuotes #-}
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
  args <- sequence (hMapM (Fun progArgExpr :: Fun CallArgumentGen (Q Exp)) l)
  pure [funSig, funDec args]
  where
    fName' = mkName fName
    funSig = SigD fName' (AppT (AppT ArrowT (ConT recTypeName)) (AppT ListT (ConT ''String)))
    funBody args = UInfixE (VarE 'concat) (VarE (mkName ".")) (AppE (VarE 'flap) (ListE args))
    funDec args = FunD fName' [Clause [] (NormalB $ funBody args) []]

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
