{-# LANGUAGE GADTs #-}
module Framework.Logging.StructuredLogger.Language where

import Data.Text
import Control.Monad.Free.Church

import Framework.Logging.Types


-- | Structured logger interface
data StructuredLoggerF next where
  -- | Structured logging method to report specific messages to services like Sentry.
  Report
    :: Severity
    -> LoggerName
    -> Message
    -> [Attribute]
    -> (() -> next)
    -> StructuredLoggerF next


type StructuredLoggerL = F StructuredLoggerF

instance Functor StructuredLoggerF where
  fmap f (Report sev name msg attrs next) = Report sev name msg attrs (f . next)

report
  :: Severity
  -> LoggerName
  -> Message
  -> [Attribute]
  -> StructuredLoggerL ()
report sev name msg attrs = liftF (Report sev name msg attrs id)
