{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Framework.Logging.StructuredLogger.Config where

import Data.Aeson
import GHC.Generics

import Framework.Logging.Types


data StructuredLoggerConfig = StructuredLoggerConfig
  { sLogLevel             :: LogLevel
    -- ^ Log level
  , sLogSentryDSN         :: String
    -- ^ Sentry connection string
  , sLogSentryDefaultTags :: [(String, String)]
    -- ^ Sentry-specific tags that will be attached unconditionally
  , sLogSentryLoggerName  :: String
    -- ^ Sentry logger name
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)
