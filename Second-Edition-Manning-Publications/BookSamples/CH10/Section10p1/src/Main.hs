module Main where

import Data.IORef
import Data.Text
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (bracket)

import qualified Framework.Logging.Types as Log
import qualified Framework.Logging.Logger.Language as Log
import qualified Framework.Logging.Logger.Config as Cfg
import qualified Framework.Logging.Logger.Impl.ConsoleLogger1 as Log1
import qualified Framework.Logging.Logger.Impl.ConsoleLogger2 as Log2
import qualified Framework.Logging.Logger.Runtime as R


{-# NOINLINE staticLoggerRef #-}
staticLoggerRef :: IORef (Log.LoggerL () -> IO ())
staticLoggerRef = unsafePerformIO $ newIORef Log1.runLogger

staticLogInfo :: Text -> IO ()
staticLogInfo msg = do
  staticLogger <- readIORef staticLoggerRef
  staticLogger $ Log.logMessage Log.Info msg


main :: IO ()
main = do
  staticLogInfo "Hi there!"
  writeIORef staticLoggerRef Log2.runLogger
  staticLogInfo "Hi there!"


  let loggerCfg = Cfg.LoggerConfig
        { Cfg.logFormat    = "<some format here>"
        , Cfg.logLevel     = Log.Info
        , Cfg.logFilePath  = ""
        , Cfg.logToConsole = True
        , Cfg.logToFile    = False
        }

  bracket (R.initLogger loggerCfg) R.disposeLogger $ \loggerRt ->
    logTestMessage loggerRt

logTestMessage loggerRt
  = R.loggerImpl loggerRt
  $ Log.logMessage Log.Info "Hi there!"
