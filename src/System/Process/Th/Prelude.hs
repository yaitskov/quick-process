module System.Process.Th.Prelude (module M, liftIO1) where

import Data.Char as M (isAlphaNum, isAlpha, isLetter)
import Data.Set as M (member)
import Generic.Random as M (genericArbitraryU)
import Relude as M hiding (Predicate)
import Test.QuickCheck as M (Arbitrary (..))

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO
