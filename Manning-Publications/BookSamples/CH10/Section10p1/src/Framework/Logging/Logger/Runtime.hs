module Framework.Logging.Logger.Runtime where

import qualified Data.Text.IO as T
import Control.Monad.Free.Church
import Control.Monad

import Framework.Logging.Types
import qualified Framework.Logging.Logger.Config as Cfg
import Framework.Logging.Logger.Language
import qualified Framework.Logging.Logger.Impl.ConsoleLogger1 as CLog1
import qualified Framework.Logging.Logger.Impl.FileLogger as FLog


-- A very simple approach to logger implementation.
-- You can find a more developed approach in the Hydra framework:
-- https://github.com/graninas/Hydra

data LoggerRuntime = LoggerRuntime
  { loggerConfig  :: Cfg.LoggerConfig
  , loggerImpl    :: LoggerL () -> IO ()
  }

initLogger
  :: Cfg.LoggerConfig
  -> IO LoggerRuntime
initLogger cfg = do
  let consoleLogger =
        if Cfg.logToConsole cfg
          then CLog1.runLogger
          else (\_ -> pure ())
  let fileLogger =
        if Cfg.logToFile cfg
          then FLog.runLogger (Cfg.logFilePath cfg)
          else (\_ -> pure ())

  let logger act = do
        consoleLogger act
        fileLogger act

  pure $ LoggerRuntime
    { loggerConfig = cfg
    , loggerImpl = logger
    }


disposeLogger :: LoggerRuntime -> IO ()
disposeLogger _ = pure ()         -- nothing to do here
