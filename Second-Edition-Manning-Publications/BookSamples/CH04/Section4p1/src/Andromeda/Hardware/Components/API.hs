module Andromeda.Hardware.Components.API where

import Andromeda.Hardware.Common


data SensorAPI = SensorAPI
  { reset :: IO ()
  , readMeasurement :: IO Measurement
  , setCallback :: Period -> IO Measurement -> IO ()
  }

data ControllerAPI = ControllerAPI
  { reboot :: IO ()
  , turnOff :: IO ()
  , eval :: String -> IO ()
  , doSomethingElse :: IO ()
  }
