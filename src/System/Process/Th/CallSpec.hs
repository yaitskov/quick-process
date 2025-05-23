module System.Process.Th.CallSpec
  ( genCallSpec
  , module E
  ) where

import Data.HList
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax qualified as THS
import System.Process.Th.CallArgument
import System.Process.Th.CallSpec.Type as E
import System.Process.Th.Prelude
import Text.Casing
import Text.Regex


type FoldrConstr l a = (HFoldr (Mapcar (Fun CallArgumentGen (Q a))) [Q a] l [Q a])

dataD' :: Quote m => Name -> [m Con] -> [m DerivClause] -> m Dec
dataD' name = dataD (pure []) name [] Nothing

genCallArgsRecord :: (Show (HList l), FoldrConstr l (Maybe VarBangType)) => Name -> HList l -> Q Dec
genCallArgsRecord recordName l = do
  fields <- seqA $ catMaybes <$> sequence (hMapM fieldDef l)
  dataD' recordName [recC recordName fields]
    [derivClause Nothing [[t|Typeable|], [t|Data|], [t|Generic|], [t|Show|], [t|Eq|]]]
  where
    fieldDef = Fun fieldExpr :: Fun CallArgumentGen (Q (Maybe VarBangType))

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

genArbitraryInstance :: Name -> Q Dec
genArbitraryInstance recordName =
  instanceD (pure []) [t| Arbitrary $(conT recordName) |]
    [ funD' 'arbitrary [] [| genericArbitraryU |]
    ]


genCallSpecInstance :: FoldrConstr l Exp => [VerificationMethod] -> Name -> String -> HList l  -> Q Dec
genCallSpecInstance verMethods recordName progName l =
  instanceD (pure []) [t| CallSpec $(conT recordName) |]
  [ funD' 'programName [ [p|_|] ] [| $(stringE progName) |]
  , funD' 'programArgs []
      [| concat . flap $(listE (hMapM (Fun progArgExpr :: Fun CallArgumentGen (Q Exp)) l)) |]
  , funD' 'verificationMethods [ [p|_|] ] (THS.lift verMethods)
  ]

mkName' :: NonEmptyStr -> Name
mkName' = mkName . toList

-- | gen declaration of CallSpec record with CallSpec instance
genCallSpec ::
  (FoldrConstr l (Maybe VarBangType), FoldrConstr l Exp, Show (HList l)) =>
  [VerificationMethod] -> String -> HList l -> Q [Dec]
genCallSpec verMethods progName l =
  maybe err (g . mkName') (programNameToHsIdentifier progName)
  where
    err = fail $ "Call spec name is bad: " <> show progName <> " " <> show l
    g recName =
      sequence
      [ genCallArgsRecord recName l
      , genCallSpecInstance verMethods recName progName l
      , genArbitraryInstance recName
      ]
