module System.Process.Quick.Util where

import Control.Monad.Writer.Strict hiding (lift)
import System.Exit hiding (exitFailure)
import System.Process
import System.Process.Quick.Prelude hiding (Type, lift)

type M m = (MonadTime m, MonadMask m, MonadCatch m, MonadIO m)

callProcessSilently :: M m => FilePath -> [String] -> m (Maybe Doc)
callProcessSilently p args =
  tryIO (liftIO (readProcessWithExitCode p args "")) >>= \case
    Left e ->
      pure . Just $ "Command: [" <> doc p <> " " <> hsep (escArg <$> args) <> "]" $$
      "Failed due:" $$ tab e

    Right (ExitSuccess, _, _) -> pure Nothing
    Right (ExitFailure ec, out, err) ->
      pure . Just $ "Command: [" <> doc p <> " " <> hsep (escArg <$> args) <> "]" $$
      (if ec > 1 then "Exited with: " <> show ec $$ "" else "")
      <> out &! (("Output: " <+>) . tab) <> err &! (("StdErr: " <+>) . tab)
