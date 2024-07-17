module Framework.Logging.Logger.Impl.FileLogger where

import qualified Data.Text.IO as T
import Control.Monad.Free.Church
import Control.Monad

import Framework.Logging.Types
import Framework.Logging.Logger.Language


interpretLoggerMethod
  :: FilePath
  -> LoggerF a
  -> IO a
interpretLoggerMethod logFile (LogMessage _ msg next) = do
  T.appendFile logFile msg
  pure $ next ()

runLogger
  :: FilePath
  -> LoggerL a
  -> IO a
runLogger logFile logAction =
  foldF (interpretLoggerMethod logFile) logAction

