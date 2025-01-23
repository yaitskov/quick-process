{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module System.Process.Th.CallArgument where

import Data.HList

import Language.Haskell.TH as TH
import System.Process.Th.Prelude hiding (Text)
import TH.Utilities qualified as TU

class CallArgument a where
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

class Typeable a => CallArgumentGen a where
  -- | field name in the record; costant value does not have a field
  cArgName :: a -> Maybe String
  -- | lambda expression projecting a call argument in CallSpec record to a list of strings
  -- Exp type is '\v -> [String]'
  progArgExpr :: a -> Q Exp
  -- | TH field definition of call argument in CallSpec record
  fieldExpr :: a -> Q (Maybe VarBangType)

newtype ConstArg = ConstArg String deriving (Eq, Show, Typeable)
instance CallArgumentGen ConstArg where
  cArgName _ = Nothing
  progArgExpr (ConstArg c) = pure (AppE (VarE 'const) (ListE [LitE $ StringL c]))
  fieldExpr _ = pure Nothing

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

newtype VarArg a = VarArg String deriving (Eq, Show, Typeable)
instance (Typeable a, CallArgument a) => CallArgumentGen (VarArg a) where
  cArgName (VarArg n) = Just n
  progArgExpr (VarArg fieldName) = do
    x <- newName "x"
    pure $ LamE [VarP x]
      (AppE (VarE 'maybeToList)
        (AppE (VarE 'toExecString)
          (AppE (VarE (mkName fieldName)) (VarE x))))

  fieldExpr (VarArg fieldName) =
    Just . (mkName fieldName, defaultBang,) <$> TU.typeRepToType (typeRep (Proxy @a))
