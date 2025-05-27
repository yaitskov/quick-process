{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Process.Th.CallSpec.Subcases where

import System.Process.Th.CallArgument
import System.Process.Th.CallSpec
import Data.HList
import Language.Haskell.TH as TH
import System.Process.Th.Prelude hiding (show)
import Language.Haskell.TH.Syntax (addTopDecls)
import Text.Show (Show (show))

newtype DcName = DcName { unDcName :: String } deriving (Show, Eq, Ord, Data, IsString)

data Subcase where
  Subcase ::
    forall l.
    ( FoldrConstr l (Maybe VarBangType)
    , FoldrConstr l Exp
    , Show (HList l)
    ) => DcName -> HList l -> Subcase

instance Show Subcase where
  show (Subcase dc l) = show $ "Subcase (" <> show dc <> ") " <> show l

newtype TcName = TcName { unTcName :: String } deriving (Show, Eq, Ord, Data, IsString)

data Subcases
  = Subcases
    { tcName :: TcName
    , subcases :: [Subcase]
    } deriving (Show)

subcaseToRecC :: Subcase -> Q TH.Con
subcaseToRecC (Subcase (DcName dcName) l) = do
  fields <- seqA $ catMaybes <$> sequence (hMapM fieldDef l)
  recC (mkName dcName) fields
  where
    fieldDef = Fun fieldExpr :: Fun CallArgumentGen (Q (Maybe VarBangType))

subcasesToDec :: Name -> [Subcase] -> Q Dec
subcasesToDec tyCon cases = do
  dataD'
    tyCon
    (fmap subcaseToRecC cases)
    [derivClause Nothing [[t|Data|], [t|Generic|], [t|Show|], [t|Eq|]]]

subcaseToClause :: Subcase -> Q Clause
subcaseToClause (Subcase (DcName dcName) l) = do
  x <- newName "x"
  f <- [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (Q Exp)) l)) |]
  pure $ Clause
    [AsP x (RecP (mkName dcName) [])]
    (NormalB (AppE f (VarE x)))
    []

instance CallArgumentGen Subcases where
  cArgName = Just . mapFirst toLower . unTcName . tcName
  progArgExpr (Subcases (TcName tyCon) cases) = do
    addTopDecls =<< sequence [ subcasesToDec (mkName tyCon) cases
                             , genArbitraryInstance (mkName tyCon)
                             ]
    LamCasesE <$> mapM subcaseToClause cases

  fieldExpr (Subcases (TcName tyCon) _) =
    pure $ Just ( mkName $ mapFirst toLower tyCon
                , defaultBang
                , ConT $ mkName tyCon
                )
