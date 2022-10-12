{-# LANGUAGE DeriveAnyClass #-}
module Framework.Logger.Types where

import Data.Aeson
import Data.Text
import Control.Monad.Free.Church
import GHC.Generics


data LogLevel = Debug | Info | Warning | Error
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

type Message = Text

