module System.Process.Th.Prelude (module M, liftIO1) where

import Control.Exception.Safe as M (MonadMask, MonadCatch, bracket, tryIO, try, tryAny)
import Data.Data as M (Data, gmapM)
import Data.Char as M (isAlphaNum, isAlpha, isLetter, toLower)
import Data.HList as M (typeRep)
import Data.List as M (isSuffixOf)
import Data.Set as M (member)
import Generic.Random as M (genericArbitraryU)
import Relude as M hiding (Predicate)
import Relude.Extra as M (toPairs)
import Test.QuickCheck as M (Gen, Arbitrary (..), generate, chooseInt, sized, elements)
import System.Process.Th.Pretty as M
import Refined as M (Refined, unrefine, refine, Predicate (..), throwRefineOtherException)
import GHC.TypeLits as M (Symbol, KnownSymbol (..), symbolVal)

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO
