module System.Process.Th.CallArgument where

import Data.HList
import Language.Haskell.TH as TH
import Refined as M
import System.Process.Th.Prelude hiding (Text)
import TH.Utilities qualified as TU

class Arbitrary a => CallArgument a where
  toExecString :: a -> Maybe String
  default toExecString :: Show a => a -> Maybe String
  toExecString = Just . show

instance CallArgument String where
  toExecString = Just
instance CallArgument a => CallArgument (Maybe a) where
  toExecString = (toExecString =<<)
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
  toExecString _ = Nothing

instance (Typeable a, Predicate c a, CallArgument a) => CallArgument (Refined c a) where
  toExecString = toExecString . unrefine

class (Typeable a) => CallArgumentGen a where
  -- | field name in the record; constant value does not have a field
  cArgName :: a -> Maybe String
  -- | lambda expression projecting a call argument in CallSpec record to a list of strings
  -- Exp type is '\v -> [String]'
  progArgExpr :: a -> Q Exp
  -- | TH field definition of call argument in CallSpec record
  fieldExpr :: a -> Q (Maybe VarBangType)

newtype ConstArg = ConstArg String deriving (Eq, Show, Typeable)
instance CallArgumentGen ConstArg where
  cArgName _ = Nothing
  progArgExpr (ConstArg c) = [| const [ $(stringE c)] |]
  fieldExpr _ = pure Nothing

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


newtype VarArg a = VarArg String deriving (Eq, Show, Typeable)
instance (Typeable a, CallArgument a) => CallArgumentGen (VarArg a) where
  cArgName (VarArg n) = Just n
  progArgExpr (VarArg fieldName) =
    [| maybeToList . toExecString . $(nameE fieldName) |]

  fieldExpr (VarArg fieldName) =
    Just . (mkName $ escapeFieldName fieldName, defaultBang,) <$> TU.typeRepToType (typeRep (Proxy @a))
