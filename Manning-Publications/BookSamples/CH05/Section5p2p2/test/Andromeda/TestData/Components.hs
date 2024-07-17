module Andromeda.TestData.Components where

import Andromeda.Hardware
import qualified Andromeda.Hardware.Impl.Device.Types as D

import qualified Data.Map as Map

thermometer1Passp :: ComponentPassport
thermometer1Passp =
  ComponentPassport (Sensors Temperature) "t1" "t1" "t1"

pressure1Passp :: ComponentPassport
pressure1Passp =
  ComponentPassport (Sensors Pressure) "p1" "p1" "p1"

controller1Passp :: ComponentPassport
controller1Passp =
  ComponentPassport Controllers "controller" "controller" "controller"


thermometer1Handler :: SensorAPI
thermometer1Handler = SensorAPI
  { reset           = putStrLn "t1 reset."
  , readMeasurement = pure (Measurement Temperature 50.0)
  , setCallback     = \_ _ -> putStrLn "t1 callback."
  }

pressure1Handler :: SensorAPI
pressure1Handler = SensorAPI
  { reset           = putStrLn "p1 reset."
  , readMeasurement = pure (Measurement Pressure 2.0)
  , setCallback     = \_ _ -> putStrLn "p1 callback."
  }
