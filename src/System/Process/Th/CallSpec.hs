module System.Process.Th.CallSpec
  ( FoldrConstr
  , genCallSpec
  , genArbitraryInstance
  , dataD'
  , seqA
  , programNameToHsIdentifier
  , module E
  ) where

import Control.Monad.Writer.Strict
import Data.HList
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax qualified as THS
import System.Directory
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec.Type as E
import System.Process.Th.Prelude
import Text.Casing
import Text.Regex

type FoldrConstr l a = (HFoldr (Mapcar (Fun CallArgumentGen (QR a))) [QR a] l [QR a])

dataD' :: Quote m => Name -> [m Con] -> [m DerivClause] -> m Dec
dataD' name = dataD (pure []) name [] Nothing

genCallArgsRecord :: (Show (HList l), FoldrConstr l (Maybe VarBangType)) => Name -> HList l -> QR Dec
genCallArgsRecord recordName l = do
  fields <- seqA $ catMaybes <$> sequence (hMapM fieldDef l)
  dataD' recordName [recC recordName fields]
    [derivClause Nothing [[t|Data|], [t|Generic|], [t|Show|], [t|Eq|]]]
  where
    fieldDef = Fun fieldExpr :: Fun CallArgumentGen (QR (Maybe VarBangType))

funD' :: Quote m => Name -> [m Pat] -> m Exp -> m Dec
funD' fname fparams fbody =
  funD fname [clause fparams (normalB fbody) []]

type NonEmptyStr = NonEmpty Char

programNameToHsIdentifier :: String -> Maybe (NonEmpty Char)
programNameToHsIdentifier = nonEmpty . toPascal . fromSnake . underbarred
  where
    underbarred s = subRegex (mkRegex "[^A-Za-z0-9_]") s "_"

seqA :: Monad m => m [a] -> m [m a]
seqA = (fmap pure <$>)

genArbitraryInstance :: Name -> QR Dec
genArbitraryInstance recordName =
  instanceD (pure []) [t| Arbitrary $(conT recordName) |]
    [ funD' 'arbitrary [] [| genericArbitraryU |]
    ]

genCallSpecInstance :: FoldrConstr l Exp => [VerificationMethod] -> Name -> String -> HList l -> QR Dec
genCallSpecInstance verMethods recordName progName l =
  instanceD (pure []) [t| CallSpec $(conT recordName) |]
  [ funD' 'programName [ [p|_|] ] [| $(stringE progName) |]
  , funD' 'programArgs []
      [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (QR Exp)) l)) |]
  , funD' 'verificationMethods [ [p|_|] ] (THS.lift $ sort verMethods)
  ]

mkName' :: NonEmptyStr -> Name
mkName' = mkName . toList

-- | gen declaration of CallSpec record with CallSpec instance
genCallSpec ::
  (FoldrConstr l (Maybe VarBangType), FoldrConstr l Exp, Show (HList l)) =>
  [VerificationMethod] -> String -> HList l -> Q [Dec]
genCallSpec verMethods progName l = do
  runIO . whenNothingM_ (findExecutable progName) . fail $ "Program " <> show progName <> " is not found"
  maybe err (g . mkName') (programNameToHsIdentifier progName)
  where
    err = fail $ "Call spec name is bad: " <> show progName <> " " <> show l
    g recName = do
      (a, w) <- runWriterT . unQR $ sequence
        [ genCallArgsRecord recName l
        , genCallSpecInstance verMethods recName progName l
        , genArbitraryInstance recName
        ]
      pure $ w <> a
