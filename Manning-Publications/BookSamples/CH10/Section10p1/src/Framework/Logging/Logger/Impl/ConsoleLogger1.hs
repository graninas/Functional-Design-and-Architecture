module Framework.Logging.Logger.Impl.ConsoleLogger1 where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.Free.Church
import Control.Monad

import Framework.Logging.Types
import Framework.Logging.Logger.Language


interpretLoggerMethod
  :: LoggerF a
  -> IO a
interpretLoggerMethod (LogMessage lvl msg next) = do
  T.putStrLn $ "[" <> T.pack (show lvl) <> "] " <> msg
  pure $ next ()


runLogger
  :: LoggerL a
  -> IO a
runLogger logAction = foldF interpretLoggerMethod logAction
