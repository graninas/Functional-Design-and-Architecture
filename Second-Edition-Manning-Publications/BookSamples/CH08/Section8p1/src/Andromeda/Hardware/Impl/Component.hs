module Andromeda.Hardware.Impl.Component where

import Andromeda.Hardware.Common

import Data.Map (Map)
import Andromeda.Common.Physics

-- This is just a demo of API.
-- We'll redesign it later.

data SensorAPI = SensorAPI
  { reset :: IO ()
  , readMeasurement :: IO SensorMeasurement
  , setCallback :: Period -> IO SensorMeasurement -> IO ()
  }

data ControllerAPI = ControllerAPI
  { reboot :: IO ()
  , turnOff :: IO ()
  , eval :: String -> IO ()
  , doSomethingElse :: IO ()
  }


data VendorComponent
  = VendoredSensor     ComponentPassport SensorAPI
  | VendoredController ComponentPassport ControllerAPI


type VendorComponents = Map ComponentName VendorComponent
