{-# LANGUAGE OverloadedStrings #-}
module Framework.Logging.StructuredLogger.Impl.Sentry where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Control.Monad.Free.Church
import qualified System.Log.Raven as R
import qualified System.Log.Raven.Types as R
import qualified System.Log.Raven.Transport.HttpConduit as R

import qualified Framework.Logging.Types as T
import qualified Framework.Logging.StructuredLogger.Config as Cfg
import qualified Framework.Logging.StructuredLogger.Language as L
import qualified Framework.Logging.StructuredLogger.Fields.Sentry as F


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
interpretSentryLoggerMethod service (L.LogStructured severity loggerName msg attribs next) = do
  timestamp <- case Map.lookup F.sentryTimestampKey attribs of
    Nothing -> getCurrentTime
    Just tsVal -> case A.eitherDecode $ LBS.fromStrict $ T.encodeUtf8 tsVal of
      Left err -> error err
      Right ts -> pure ts
  let sSeverity = toSentrySeverity severity
  let sMsg = T.unpack msg
  let sLoggerName = T.unpack loggerName
  let sSentryRecordF = toSentryFields timestamp $ Map.toList attribs
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


toSentryFields
  :: T.Timestamp
  -> [(T.FieldKey, T.FieldValue)]
  -> R.SentryRecord
  -> R.SentryRecord
toSentryFields ts [] r = r { R.srTimestamp = ts}
toSentryFields ts ((attrKey, attrVal):attrs) r = let
  unpackedVal = T.unpack attrVal
  r' = case () of
        _ | attrKey == F.sentryEventIdKey     -> r { R.srEventId     = unpackedVal      }
        _ | attrKey == F.sentryPlatformKey    -> r { R.srPlatform    = Just unpackedVal }
        _ | attrKey == F.sentryServerNameKey  -> r { R.srServerName  = Just unpackedVal }
        _ | attrKey == F.sentryCulpritKey     -> r { R.srCulprit     = Just unpackedVal }
        _ | attrKey == F.sentryReleaseKey     -> r { R.srRelease     = Just unpackedVal }
        _ | attrKey == F.sentryEnvironmentKey -> r { R.srEnvironment = Just unpackedVal }
        _ | otherwise -> r {R.srExtra = HM.insert (T.unpack attrKey) (A.String attrVal) (R.srExtra r) }

  in toSentryFields ts attrs r'




-- A simplified pseudocode for the book.
--

-- interpretSentryLoggerMethod'
--   :: R.SentryService
--   -> L.StructuredLoggerF a
--   -> IO a
-- interpretSentryLoggerMethod' service (L.LOgStructured severity loggerName msg attribs next) = do
--   let sSeverity = toSentrySeverity severity
--   let sMsg = T.unpack msg
--   let sLoggerName = T.unpack loggerName
--   let sentryRecord = toSentryRecord attribs R.blankSentryRecord
--   R.register service sLoggerName sSeverity sMsg sSentryRecordF
--   pure $ next ()

-- toSentryRecord
--   :: Map.Map T.FieldKey T.FieldValue
--   -> R.SentryRecord
--   -> R.SentryRecord
-- toSentryRecord m r = foldr toSentryRecord' r (Map.toList m)

-- toSentryRecord'
--   :: (T.FieldKey, T.FieldValue)
--   -> R.SentryRecord
--   -> R.SentryRecord
-- toSentryRecord' (attrKey, attrVal) r

--   | attrKey == F.sentryEventIdKey   = r { R.srEventId   = unpackedVal }
--   | attrKey == F.sentryTimestampKey = r { R.srTimestamp = parsedTs    }

--   where
--     unpackedVal :: String
--     unpackedVal = T.unpack attrVal

--     parsedTs :: UTCTime
--     parsedTs = error "not implemented"    -- parse here










