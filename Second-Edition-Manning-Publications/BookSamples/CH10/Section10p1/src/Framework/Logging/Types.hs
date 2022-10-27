{-# LANGUAGE DeriveAnyClass #-}
module Framework.Logging.Types where

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Time


data LogLevel = Debug | Info | Warning | Error
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

type Message = Text


type LoggerName = Text
type Severity = LogLevel
type FieldKey = Text
type FieldValue = Text
type Field = (FieldKey, FieldValue)
type Timestamp = UTCTime


field  :: FieldKey -> FieldValue -> Field
field k v = (k, v)
