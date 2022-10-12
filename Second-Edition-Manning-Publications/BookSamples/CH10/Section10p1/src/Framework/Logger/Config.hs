{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
module Framework.Logger.Config where

import Data.Aeson
import GHC.Generics


import Framework.Logger.Types


data LoggerConfig = LoggerConfig
  { logFormat    :: String
  , logLevel     :: LogLevel
  , logFilePath  :: FilePath
  , logToConsole :: Bool
  , logToFile    :: Bool
  }
  deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)