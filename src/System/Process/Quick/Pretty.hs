module System.Process.Quick.Pretty
  ( Pretty (..)
  , (&!)
  , escArg
  , tab
  , printDoc
  , apNe
  , module PP
  ) where

import Control.Exception.Safe
import GHC.ResponseFile (escapeArgs)
import Relude
import Text.PrettyPrint as PP hiding (hsep, (<>), empty, isEmpty)
import Text.PrettyPrint qualified as PP


class Pretty a where
  default doc :: Show a => a -> Doc
  doc = text . show
  doc :: a -> Doc

  hsep :: [a] -> Doc
  hsep = PP.hsep . fmap doc
  {-# INLINE hsep #-}

  vsep :: [a] -> Doc
  vsep = vcat . fmap doc
  {-# INLINE vsep #-}

instance Pretty Doc where
  doc = id
  {-# INLINE doc #-}
instance Pretty String where
  doc = text
  {-# INLINE doc #-}
instance Pretty IOException
instance Pretty Int
instance Pretty Integer
instance Pretty [String]


printDoc :: MonadIO m => Doc -> m ()
printDoc = putStrLn . render

tab :: Pretty a => a -> Doc
tab = nest 2 . doc

class IsEmpty a where
  isEmpty :: a -> Bool

instance IsEmpty [a] where
  isEmpty = null

apNe :: (IsEmpty a, Pretty a) => a -> (Doc -> Doc) -> Doc
apNe d f
  | isEmpty d = d'
  | otherwise = f d'
  where
    d' = doc d

(&!) :: (IsEmpty a, Pretty a) => a -> (Doc -> Doc) -> Doc
(&!) = apNe

infixl 7 &!

escArg :: String -> String
escArg = reverse . drop 1 . reverse . escapeArgs . pure
