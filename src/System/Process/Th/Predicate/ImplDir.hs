module System.Process.Th.Predicate.ImplDir where

import System.Process.Th.Prelude


data ImplDir (localDirPath :: Symbol) deriving (Data, Show, Eq, Generic)
