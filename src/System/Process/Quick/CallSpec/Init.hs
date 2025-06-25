module System.Process.Quick.CallSpec.Init where

import Language.Haskell.TH as TH
import System.Process.Quick.CallArgument
import System.Process.Quick.CallSpec
import System.Process.Quick.Prelude
import TH.Utilities qualified as TU

data Init cs = Init deriving (Show, Eq, Ord, Data)

instance CallSpec cs => CallArgumentGen (Init cs) where
  cArgName _ = Nothing
  -- Exp type is '\v -> [String]'
  progArgExpr _ = [| const [] |]
  fieldExpr _ = pure Nothing
  -- | Exp type is '\v -> m [CsBox]'
  initCallSpecsExpr _ = do
    -- [| const (sequence [generate (arbitrary @cs)]) |]
    trep <- QR $ lift $ TU.typeRepToType (typeRep (Proxy @cs))
    pure (AppE (VarE 'const)
          (AppE (VarE 'liftIO)
            (AppE (VarE 'sequence)
              (ListE
                [ AppE
                  (AppE (VarE 'fmap) (ConE 'CsBox))
                  (AppE (VarE 'generate)
                    (AppTypeE (VarE 'arbitrary) trep))
                ]))))
