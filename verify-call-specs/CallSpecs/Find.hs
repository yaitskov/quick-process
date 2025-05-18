-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module CallSpecs.Find where

import CallSpecs.Find.Type
import System.Process.Th
import System.Process.Th.Predicate.Regex
import System.Process.Th.Prelude hiding (NonEmpty, Type)

type DirPath = Refined FsPath String

$(genCallSpec "find" (ConstArg "-H" .*. VarArg @DirPath "path" .*. VarArg @NodeType "-type" .*. HNil))
