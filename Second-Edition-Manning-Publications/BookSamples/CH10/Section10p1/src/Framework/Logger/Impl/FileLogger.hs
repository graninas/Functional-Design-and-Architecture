module Framework.Logger.Impl.FileLogger where

import qualified Data.Text.IO as T
import Control.Monad.Free.Church
import Control.Monad

import Framework.Logger.Language



data FileLoggerRuntime = FileLoggerRuntime
  { logFile :: String
  , logLevel :: LogLevel
  }


interpretLoggerMethod
  :: FileLoggerRuntime
  -> LoggerF a
  -> IO a
interpretLoggerMethod loggerRt (LogMessage _ msg next) = do
  T.appendFile (logFile loggerRt) msg
  pure $ next ()




runLogger
  :: FileLoggerRuntime
  -> LoggerL a
  -> IO a
runLogger loggerRt logAction =
  foldF (interpretLoggerMethod loggerRt) logAction
