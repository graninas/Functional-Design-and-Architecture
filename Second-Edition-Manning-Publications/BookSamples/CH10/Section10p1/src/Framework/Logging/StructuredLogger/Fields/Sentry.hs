module Framework.Logging.StructuredLogger.Fields.Sentry where

import Data.Text
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T

import Framework.Logging.Types


sentryEventIdKey :: FieldKey
sentryEventIdKey = "srEventId"

sentryEventId :: Text -> Field
sentryEventId v = field sentryEventIdKey v

sentryPlatformKey :: FieldKey
sentryPlatformKey = "srPlatform"

sentryPlatform :: Text -> Field
sentryPlatform v = field sentryPlatformKey v

sentryServerNameKey :: FieldKey
sentryServerNameKey = "srServerName"

sentryServerName :: FieldValue -> Field
sentryServerName v = field sentryServerNameKey v

sentryCulpritKey :: FieldKey
sentryCulpritKey = "srCulprit"

sentryCulprit :: Text -> Field
sentryCulprit v = field sentryCulpritKey v

sentryReleaseKey :: FieldKey
sentryReleaseKey = "srRelease"

sentryRelease :: Text -> Field
sentryRelease v = field sentryReleaseKey v

sentryEnvironmentKey :: FieldKey
sentryEnvironmentKey = "srEnvironment"

sentryEnvironment :: Text -> Field
sentryEnvironment v = field sentryEnvironmentKey v

sentryTimestampKey :: FieldKey
sentryTimestampKey = "srTimestamp"

sentryTimestamp :: Timestamp -> Field
sentryTimestamp ts = field sentryTimestampKey
  (T.decodeUtf8 $ LBS.toStrict $ A.encode ts)
