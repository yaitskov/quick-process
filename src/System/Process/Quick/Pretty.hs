{-# OPTIONS_GHC -Wno-orphans #-}
module System.Process.Quick.Pretty
  ( doc
  , hsep -- Pretty (..)
  , vsep
  , (&!)
  , ($$)
  , escArg
  , tab
  , printDoc
  , apNe
  , module PP
  ) where

import Control.Exception ( IOException )
-- import Control.Exception.Safe
import GHC.ResponseFile (escapeArgs)
import Relude
-- import Text.PrettyPrint as PP hiding (hsep, (<>), empty, isEmpty)
-- import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.Leijen.Text as PP hiding ((<$>), bool, group, hsep, vsep, empty, isEmpty)
import Text.PrettyPrint.Leijen.Text qualified as PP

infixr 5 $$
($$) :: Doc -> Doc -> Doc
($$) = (<$$>)

-- class Pretty a where
--   default doc :: Show a => a -> Doc
doc :: Pretty a => a -> Doc
doc = pretty --  . show
-- doc :: a -> Doc

hsep :: Pretty a => [a] -> Doc
hsep = PP.hsep . fmap doc
-- {-# INLINE hsep #-}

vsep :: Pretty a => [a] -> Doc
vsep = vcat . fmap doc
-- {-# INLINE vsep #-}

-- instance Pretty Doc where
--   doc = id
--   {-# INLINE doc #-}
-- instance Pretty String where
--   doc = text
--   {-# INLINE doc #-}
-- instance Pretty IOException
-- instance Pretty Int
-- instance Pretty Integer
-- instance Pretty [String]


-- printDoc :: MonadIO m => Doc -> m ()
-- printDoc :: a -> m ()
printDoc :: (MonadIO m, Pretty a) => a -> m ()
printDoc x = liftIO (putDoc $ pretty x)

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

instance Pretty IOException where
  pretty = text . show
