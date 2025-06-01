module System.Process.Quick.Predicate.ImplDir where

import System.Process.Quick.Prelude


data ImplDir (localDirPath :: Symbol) deriving (Data, Show, Eq, Generic)
