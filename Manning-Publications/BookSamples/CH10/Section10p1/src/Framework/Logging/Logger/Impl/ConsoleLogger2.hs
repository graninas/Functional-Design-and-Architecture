module Framework.Logging.Logger.Impl.ConsoleLogger2 where

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
  T.putStrLn $ msg <> " <<<" <> T.pack (show lvl) <> ">>>"
  pure $ next ()


runLogger
  :: LoggerL a
  -> IO a
runLogger logAction = foldF interpretLoggerMethod logAction
