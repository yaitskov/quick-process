module System.Process.Th.Prelude (module M, liftIO1) where

import Relude as M hiding (Predicate)

liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO
