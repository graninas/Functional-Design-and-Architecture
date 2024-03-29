module Andromeda.Hardware.Impl.Component where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain (Status)

import Data.Map (Map)


-- This is just a demo of API.
-- We'll redesign it later.

data SensorAPI = SensorAPI
  { reset :: IO ()
  , readMeasurement :: IO Measurement
  , setCallback :: Period -> IO Measurement -> IO ()
  }

data ControllerAPI = ControllerAPI
  { getStatus :: IO Status
  , reboot :: IO ()
  , turnOff :: IO ()
  , eval :: String -> IO ()
  }


data VendorComponent
  = VendoredSensor     ComponentPassport SensorAPI
  | VendoredController ComponentPassport ControllerAPI


type VendorComponents = Map ComponentName VendorComponent
