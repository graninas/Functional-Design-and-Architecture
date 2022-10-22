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
type AttributeKey = Text
type AttributeValue = Text
data Attribute = Attribute AttributeKey AttributeValue
  deriving (Show, Eq, Ord)

type Timestamp = UTCTime
