module Framework.Logging.StructuredLogger.Fields where

import Framework.Logging.Types

sentryEventIdKey :: AttributeKey
sentryEventIdKey = "srEventId"

sentryEventId :: AttributeValue -> Attribute
sentryEventId v = Attribute sentryEventIdKey v

sentryPlatformKey :: AttributeKey
sentryPlatformKey = "srPlatform"

sentryPlatform :: AttributeValue -> Attribute
sentryPlatform v = Attribute sentryPlatformKey v

sentryServerNameKey :: AttributeKey
sentryServerNameKey = "srServerName"

sentryServerName :: AttributeValue -> Attribute
sentryServerName v = Attribute sentryServerNameKey v

sentryCulpritKey :: AttributeKey
sentryCulpritKey = "srCulprit"

sentryCulprit :: AttributeValue -> Attribute
sentryCulprit v = Attribute sentryCulpritKey v

sentryReleaseKey :: AttributeKey
sentryReleaseKey = "srRelease"

sentryRelease :: AttributeValue -> Attribute
sentryRelease v = Attribute sentryReleaseKey v

sentryEnvironmentKey :: AttributeKey
sentryEnvironmentKey = "srEnvironment"

sentryEnvironment :: AttributeValue -> Attribute
sentryEnvironment v = Attribute sentryEnvironmentKey v
