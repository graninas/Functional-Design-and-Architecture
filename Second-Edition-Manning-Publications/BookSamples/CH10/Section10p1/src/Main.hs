module Main where

import Data.IORef
import Data.Text
import System.IO.Unsafe (unsafePerformIO)

import qualified Framework.Logger.Language as Log
import qualified Framework.Logger.Impl.ConsoleLogger1 as Log1
import qualified Framework.Logger.Impl.ConsoleLogger2 as Log2


{-# NOINLINE loggerRef #-}
loggerRef :: IORef (Log.LoggerL () -> IO ())
loggerRef = unsafePerformIO $ newIORef Log1.runLogger

logInfo :: Text -> IO ()
logInfo msg = do
  logger <- readIORef loggerRef
  logger $ Log.logMessage Log.Info msg


main :: IO ()
main = do
  logInfo "Hi there!"
  writeIORef loggerRef Log2.runLogger
  logInfo "Hi there!"