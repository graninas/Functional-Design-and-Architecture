{-# LANGUAGE OverloadedStrings #-}
module Framework.Logging.StructuredLogger.Impl.Sentry where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Free.Church
import qualified System.Log.Raven as R
import qualified System.Log.Raven.Types as R
import qualified System.Log.Raven.Transport.HttpConduit as R

import qualified Framework.Logging.Types as T
import qualified Framework.Logging.StructuredLogger.Config as Cfg
import qualified Framework.Logging.StructuredLogger.Language as L
import qualified Framework.Logging.StructuredLogger.Fields as F


initSentry :: Cfg.StructuredLoggerConfig -> IO R.SentryService
initSentry cfg = do
  let defaultTags = HM.fromList (Cfg.sLogSentryDefaultTags cfg)
  let tagsF r = r { R.srTags = HM.union defaultTags (R.srTags r) }
  R.initRaven (Cfg.sLogSentryDSN cfg) tagsF R.sendRecord R.stderrFallback

disposeSentry :: R.SentryService -> IO ()
disposeSentry _ = pure ()         -- nothing to do here

interpretSentryLoggerMethod
  :: R.SentryService
  -> L.StructuredLoggerF a
  -> IO a
interpretSentryLoggerMethod service (L.Report severity loggerName msg attribs next) = do
  timestamp <- getCurrentTime
  let sSeverity = toSentrySeverity severity
  let sMsg = T.unpack msg
  let sLoggerName = T.unpack loggerName
  let sSentryRecordF = toSentryAttributes timestamp attribs
  R.register service sLoggerName sSeverity sMsg sSentryRecordF
  pure $ next ()

runSentryLogger
  :: R.SentryService
  -> L.StructuredLoggerL a
  -> IO a
runSentryLogger service logAction =
  foldF (interpretSentryLoggerMethod service) logAction


toSentrySeverity :: T.Severity -> R.SentryLevel
toSentrySeverity T.Debug    = R.Debug
toSentrySeverity T.Info     = R.Info
toSentrySeverity T.Warning  = R.Warning
toSentrySeverity T.Error    = R.Error


toSentryAttributes
  :: T.Timestamp
  -> [T.Attribute]
  -> (R.SentryRecord -> R.SentryRecord)
toSentryAttributes ts [] r = r { R.srTimestamp = ts}
toSentryAttributes ts (T.Attribute attrKey attrVal:attrs) r = let
  unpackedVal = T.unpack attrVal
  r' = case () of
        _ | attrKey == F.sentryEventIdKey     -> r { R.srEventId     = unpackedVal      }
        _ | attrKey == F.sentryPlatformKey    -> r { R.srPlatform    = Just unpackedVal }
        _ | attrKey == F.sentryServerNameKey  -> r { R.srServerName  = Just unpackedVal }
        _ | attrKey == F.sentryCulpritKey     -> r { R.srCulprit     = Just unpackedVal }
        _ | attrKey == F.sentryReleaseKey     -> r { R.srRelease     = Just unpackedVal }
        _ | attrKey == F.sentryEnvironmentKey -> r { R.srEnvironment = Just unpackedVal }
        _ | otherwise -> r {R.srExtra = HM.insert (T.unpack attrKey) (A.String attrVal) (R.srExtra r) }

  in toSentryAttributes ts attrs r'




