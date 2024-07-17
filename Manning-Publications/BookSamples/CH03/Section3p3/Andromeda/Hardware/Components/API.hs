module Andromeda.Hardware.Components.API where

import Andromeda.Hardware.Common


-- This is just a demo of API.
-- We'll design it later...

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
