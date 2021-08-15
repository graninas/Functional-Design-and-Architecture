module Andromeda.TestData.Components where

import Andromeda.Hardware
import qualified Andromeda.Hardware.Impl.Device.Types as D

import qualified Data.Map as Map

thermometer1Passp :: ComponentPassport
thermometer1Passp =
  ComponentPassport (Sensors Temperature) "1" "1" "1"


thermometer1Handler :: SensorAPI
thermometer1Handler = SensorAPI
  { reset = putStrLn "1 reset."
  , readMeasurement = pure (Measurement Temperature 50.0)
  , setCallback = \_ _ -> putStrLn "1 callback."
  }
