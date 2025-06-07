module System.Process.Quick.CallSpec.Subcases where

import Control.Monad.Writer.Strict
import System.Process.Quick.CallArgument
import System.Process.Quick.CallSpec
import Data.HList
import Language.Haskell.TH as TH
import System.Process.Quick.Prelude hiding (show)
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

subcaseToRecC :: Subcase -> QR TH.Con
subcaseToRecC (Subcase (DcName dcName) l) = do
  fields <- seqA $ catMaybes <$> sequence (hMapM fieldDef l)
  recC (mkName dcName) fields
  where
    fieldDef = Fun fieldExpr :: Fun CallArgumentGen (QR (Maybe VarBangType))

subcasesToDec :: Name -> [Subcase] -> QR Dec
subcasesToDec tyCon cases = do
  dataD'
    tyCon
    (fmap subcaseToRecC cases)
    [derivClause Nothing [[t|Data|], [t|Generic|], [t|Show|], [t|Eq|]]]

subcaseToClause :: Subcase -> QR Clause
subcaseToClause (Subcase (DcName dcName) l) = do
  x <- newName "x"
  f <- [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (QR Exp)) l)) |]
  pure $ Clause
    [AsP x (RecP (mkName dcName) [])]
    (NormalB (AppE f (VarE x)))
    []

instance CallArgumentGen Subcases where
  cArgName = Just . mapFirst toLower . unTcName . tcName
  progArgExpr (Subcases (TcName tyCon) cases) = do
    tell =<< sequence [ subcasesToDec (mkName tyCon) cases
                      , genArbitraryInstance (mkName tyCon)
                      ]
    [| $(lamCasesE (subcaseToClause <$> cases)) . $(varE . mkName $ mapFirst toLower tyCon) |]
  fieldExpr (Subcases (TcName tyCon) _) =
    pure $ Just ( mkName $ mapFirst toLower tyCon
                , defaultBang
                , ConT $ mkName tyCon
                )
  outcomeCheckersExpr (Subcases (TcName tyCon) cases) = do
    [| $(lamCasesE (subcaseToClause' <$> cases)) . $(varE . mkName $ mapFirst toLower tyCon) |]
    where
      subcaseToClause' :: Subcase -> QR Clause
      subcaseToClause' (Subcase (DcName dcName) l) = do
        x <- newName "x"
        f <- [| concat . flap $(listE (hMapM (Fun outcomeCheckersExpr :: Fun CallArgumentGen (QR Exp)) l)) |]
        pure $ Clause
          [AsP x (RecP (mkName dcName) [])]
          (NormalB (AppE f (VarE x)))
          []
