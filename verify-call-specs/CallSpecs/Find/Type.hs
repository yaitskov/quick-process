module CallSpecs.Find.Type where

import System.Process.Th
import System.Process.Th.Prelude

data NodeType = FileNode | DirNode deriving (Show, Eq, Generic, Typeable, Data)

instance Arbitrary NodeType where
  arbitrary = genericArbitraryU

instance CallArgument NodeType where
  toExecString = pure . \case
    FileNode -> "f"
    DirNode -> "d"
