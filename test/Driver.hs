module Driver where

import qualified Discovery
import System.Process.Quick.Test.Prelude

main :: IO ()
main = defaultMain =<< testTree
  where
    testTree :: IO TestTree
    testTree = do
      tests <- Discovery.tests
      pure $ testGroup "quick-process" [ tests ]
