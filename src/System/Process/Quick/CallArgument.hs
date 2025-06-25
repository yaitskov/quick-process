module System.Process.Quick.CallArgument where

import Control.Monad.Writer.Strict
import Data.HList
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax qualified as TH
import Refined as M hiding (NonEmpty)
import System.Process.Quick.CallEffect
import System.Process.Quick.OrphanArbitrary ()
import System.Process.Quick.Prelude hiding (Text)
import TH.Utilities qualified as TU

class Arbitrary a => CallArgument a where
  toExecString :: a -> [String]
  default toExecString :: Show a => a -> [String]
  toExecString = (:[]) . show

instance CallArgument a => CallArgument (Maybe a) where
  toExecString = maybe [] toExecString
instance (CallArgument a, CallArgument b) => CallArgument (Either a b) where
  toExecString = \case
    Left x -> toExecString x
    Right x -> toExecString x
instance CallArgument Int
instance CallArgument Integer
instance CallArgument Double
instance CallArgument Float
instance CallArgument Word
instance CallArgument Bool
instance CallArgument () where
  toExecString _ = []

-- | Disambiguate 'Refined.NonEmpty'
type NeList = NonEmpty

instance CallArgument a => CallArgument (NonEmpty a) where
  toExecString = concatMap toExecString

instance CallArgument a => CallArgument [a] where
  toExecString = concatMap toExecString
instance {-# OVERLAPPING #-} CallArgument String where
  toExecString = (:[])

instance (Typeable a, Predicate c a, CallArgument a) => CallArgument (Refined c a) where
  toExecString = toExecString . unrefine

newtype QR a
  = QR { unQR :: WriterT [Dec] Q a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadWriter [Dec])

instance Quote QR where
  newName n = QR $ lift (newName n)

-- data
class (Typeable a) => CallArgumentGen a where
  -- | field name in the record; constant value does not have a field
  cArgName :: a -> Maybe String
  -- | lambda expression projecting a call argument in CallSpec record to a list of strings
  -- Exp type is '\v -> [String]'
  progArgExpr :: a -> QR Exp
  -- | TH field definition of call argument in CallSpec record
  fieldExpr :: a -> QR (Maybe VarBangType)
  -- | Exp type is '\v -> [OutcomeChecker]'
  outcomeCheckersExpr :: a -> QR Exp
  outcomeCheckersExpr _ = [| const [] |]
  -- | Exp type is '\v -> m [CsBox]'
  initCallSpecsExpr :: a -> QR Exp

instance CallArgumentGen OutcomeChecker where
  cArgName _ = Nothing
  progArgExpr _ = [| const [] |]
  fieldExpr _ = pure Nothing
  outcomeCheckersExpr c =  [| pure [$(TH.lift c)] |]
  initCallSpecsExpr _ = [| pure . const [] |]

newtype ConstArg = ConstArg String deriving (Eq, Show)
instance CallArgumentGen ConstArg where
  cArgName _ = Nothing
  progArgExpr (ConstArg c) = [| const [ $(stringE c)] |]
  fieldExpr _ = pure Nothing
  initCallSpecsExpr _ = [| pure . const [] |]

newtype ConstArgs = ConstArgs [String] deriving (Eq, Show)
instance CallArgumentGen ConstArgs where
  cArgName _ = Nothing
  progArgExpr (ConstArgs cs) = [| const $(TH.lift cs) |]
  fieldExpr _ = pure Nothing
  initCallSpecsExpr _ = [| pure . const [] |]

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

nameE :: String -> Q Exp
nameE = varE . mkName . escapeFieldName

isValidFirstFieldLetter :: Char -> Bool
isValidFirstFieldLetter c = isLetter c || c == '_'

isValidFieldLetter :: Char -> Bool
isValidFieldLetter c = isAlphaNum c || c == '_' || c == '\''

haskellKeyword :: Set String
haskellKeyword = fromList [ "type", "module", "import", "where", "class", "case", "in", "of" ]

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f (h:t) = f h : t

escapeFieldName :: String -> String
escapeFieldName = \case
  [] -> error "Empty field name"
  (h:t) ->
    case filter isValidFirstFieldLetter [h] ++ filter isValidFieldLetter t of
      [] -> error "Field name " <> show (h:t) <> " is empty after filtration"
      "type" -> "type'"
      "mo" -> "type'"
      filteredFieldName
        | filteredFieldName `member` haskellKeyword -> filteredFieldName <> "'"
        | otherwise -> filteredFieldName

-- | Command line argument without preceeding key
newtype VarArg a = VarArg String deriving (Eq, Show)
instance (Typeable a, CallArgument a) => CallArgumentGen (VarArg a) where
  cArgName (VarArg n) = Just n
  progArgExpr (VarArg fieldName) =
    QR $ lift [| toExecString . $(nameE fieldName) |]

  fieldExpr (VarArg fieldName) =
    Just . (mkName $ escapeFieldName fieldName, defaultBang,) <$> atRep
    where
      atRep = QR . lift $ TU.typeRepToType (typeRep (Proxy @a))
  initCallSpecsExpr _ = [| pure . const [] |]

-- | Command line argument prefixed with a key
newtype KeyArg a = KeyArg String deriving (Eq, Show)
instance (Typeable a, CallArgument a) => CallArgumentGen (KeyArg a) where
  cArgName (KeyArg n) = cArgName (VarArg @a n)
  progArgExpr (KeyArg fieldName) =
    [| \x -> $(progArgExpr (ConstArg fieldName)) x <> $(progArgExpr (VarArg @a fieldName)) x |]
  fieldExpr (KeyArg fieldName) = fieldExpr (VarArg @a fieldName)
  initCallSpecsExpr _ = [| pure . const [] |]
