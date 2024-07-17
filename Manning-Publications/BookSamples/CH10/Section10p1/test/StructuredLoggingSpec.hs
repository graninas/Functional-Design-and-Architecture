module StructuredLoggingSpec where

import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Text as T

import Framework.Logging.StructuredLogger.Language
import Framework.Logging.StructuredLogger.Fields.Sentry
import Framework.Logging.Types


data Meteor = Meteor
  { meteorId    :: T.Text
  , size        :: Int
  , mass        :: Int
  , timestamp   :: Timestamp
  }

logMeteorObservation :: Meteor -> StructuredLoggerL ()
logMeteorObservation (Meteor mId s m ts) =
  logStructured
    Info
    "astronomer"
    (T.pack $ "Meteor found. Mass: " <> show m <> ", size: " <> show s)
    (Map.fromList
      [ sentryTimestamp ts
      , sentryEventId mId
      ])



spec :: Spec
spec = pure ()



