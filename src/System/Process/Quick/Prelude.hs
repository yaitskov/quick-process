{-# OPTIONS_HADDOCK hide #-}
module System.Process.Quick.Prelude (module M, liftIO1) where

import Control.Monad.Writer.Strict as M (MonadWriter (tell), WriterT, execWriterT)
import Control.Exception.Safe as M (MonadMask, MonadCatch, bracket, tryIO, try, tryAny, throw)
import Control.Lens as M (Lens', at, (^.), (.~), (%~), _1, _2)
import Control.Monad.Time as M (MonadTime(..))
import Data.Char as M (isAlphaNum, isAlpha, isLetter, isLower, toLower)
import Data.Data as M (Data, gmapM, gmapT)
import Data.Generics.Labels as M ()
import Data.HList as M (typeRep)
import Data.List as M (isSuffixOf)
import Data.Set as M (member)
import Data.Time.Clock as M (NominalDiffTime, diffUTCTime)
import Data.Typeable as M (TypeRep, eqT, (:~:) (Refl))
import Debug.TraceEmbrace as M (tr, tw)
import Generic.Random as M (genericArbitraryU)
import GHC.TypeLits as M (Symbol, KnownSymbol (..), symbolVal)
import Refined as M (Refined, unrefine, refine, Predicate (..), throwRefineOtherException)
import Relude as M hiding (Predicate)
import Relude.Extra as M (toPairs)
import System.Exit as M (ExitCode (..))
import System.IO.Unsafe as M (unsafePerformIO)
import System.Process as M (ProcessHandle, CreateProcess (..), readCreateProcess, readCreateProcessWithExitCode)
import System.Process.Quick.Pretty as M
import Test.QuickCheck as M (Gen, Arbitrary (..), generate, chooseInt, sized, elements, listOf)

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO
