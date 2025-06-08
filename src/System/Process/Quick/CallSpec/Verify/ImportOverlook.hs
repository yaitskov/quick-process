module System.Process.Quick.CallSpec.Verify.ImportOverlook
  ( addCompiledCallSpec
  , verifyFoundCsCoverCompiledOnes
  ) where

import Data.Set ( (\\), insert )
import Language.Haskell.TH ( Type, Name, nameModule, nameBase, mkName )
import System.Process.Quick.Prelude hiding (Type)
import System.Process.Quick.Util

erasePackage :: Type -> Type
erasePackage t = gmapT go t
  where
    go :: forall x. (Data x) => x -> x
    go x | Just Refl <- eqT @x @Name =
             mkName $ joinNe (fromMaybe "" $ nameModule x) '.' (nameBase x)
         | otherwise = x

compiledCallSpecs :: IORef (Set Type)
compiledCallSpecs = unsafePerformIO $ newIORef mempty

addCompiledCallSpec :: MonadIO m => Type -> m ()
addCompiledCallSpec xt = atomicModifyIORef'_ compiledCallSpecs $ $(tw "/") . insert (erasePackage xt)

verifyFoundCsCoverCompiledOnes :: MonadIO m => [Type] -> m (Set Type)
verifyFoundCsCoverCompiledOnes found =
  (\\ fromList found') <$> readIORef compiledCallSpecs
  where
    found' =  erasePackage <$> found
