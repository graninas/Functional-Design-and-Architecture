module Framework.Logging.StructuredLogger.Runtime where


import Framework.Logging.Types
import qualified Framework.Logging.StructuredLogger.Config as Cfg
import qualified Framework.Logging.StructuredLogger.Language as L
import qualified Framework.Logging.StructuredLogger.Impl.Sentry as S


data StructuredLoggerRuntime = StructuredLoggerRuntime
  { sLoggerConfig        :: Cfg.StructuredLoggerConfig
  , sLoggerImpl          :: L.StructuredLoggerL () -> IO ()
  , sLoggerDisposeAction :: IO ()
  }


initStructuredLogger
  :: Cfg.StructuredLoggerConfig
  -> IO StructuredLoggerRuntime
initStructuredLogger cfg = do
  service <- S.initSentry cfg
  let runAction = S.runSentryLogger service
  let disposeAction = S.disposeSentry service
  pure $ StructuredLoggerRuntime cfg runAction disposeAction

disposeStructuredLogger
  :: StructuredLoggerRuntime
  -> IO ()
disposeStructuredLogger = sLoggerDisposeAction
