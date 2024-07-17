{-# LANGUAGE GADTs #-}
module Framework.Logging.StructuredLogger.Language where

import Data.Text
import qualified Data.Map as Map
import Control.Monad.Free.Church

import Framework.Logging.Types


-- | Structured logger interface
data StructuredLoggerF next where
  -- | Structured logging method to report specific messages to services like Sentry.
  LogStructured
    :: Severity
    -> LoggerName
    -> Message
    -> Map.Map FieldKey FieldValue
    -> (() -> next)
    -> StructuredLoggerF next


type StructuredLoggerL = F StructuredLoggerF

instance Functor StructuredLoggerF where
  fmap f (LogStructured sev name msg attrs next) =
    LogStructured sev name msg attrs (f . next)

logStructured
  :: Severity
  -> LoggerName
  -> Message
  -> Map.Map FieldKey FieldValue
  -> StructuredLoggerL ()
logStructured sev name msg attrs = liftF (LogStructured sev name msg attrs id)
