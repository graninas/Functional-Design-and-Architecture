module Andromeda.Vendors.AAA where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Hdl

import qualified Data.Map as Map

-- import Native AAA sensors library here or do FFI


aaaIncVendor = "AAA Inc."


t25SensorName :: ComponentName
t25SensorName = "AAA-T-25"

p02SensorName :: ComponentName
p02SensorName = "AAA-З-02"

c86ControllerName :: ComponentName
c86ControllerName = "AAA-C-86"


t25Sensor :: ComponentDef
t25Sensor =
  ComponentDef Sensors t25SensorName "some_guid1" aaaIncVendor

p02Sensor :: ComponentDef
p02Sensor =
  ComponentDef Sensors p02SensorName "some_guid2" aaaIncVendor

c86Controller :: ComponentDef
c86Controller =
  ComponentDef Controllers c86ControllerName "some_guid3" aaaIncVendor





t25Handler :: SensorAPI
t25Handler = SensorAPI
  { reset = putStrLn $ t25SensorName <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ t25SensorName <> " callback."
  }

p02Handler :: SensorAPI
p02Handler = SensorAPI
  { reset = putStrLn $ p02SensorName <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ p02SensorName <> " callback."
  }

c86Handler :: ControllerAPI
c86Handler = ControllerAPI
  { reboot = putStrLn $ c86ControllerName <> " reset."
  , turnOff = putStrLn $ c86ControllerName <> " turn off."
  , eval = \cmd -> putStrLn $ c86ControllerName <> " eval cmd: " <> cmd
  , doSomethingElse = putStrLn $ c86ControllerName <> " do something else"
  }



aaaVendorComponents = Map.fromList
  [ ((aaaIncVendor, t25SensorName),     VendoredSensor     t25Sensor t25Handler)
  , ((aaaIncVendor, p02SensorName),     VendoredSensor     p02Sensor p02Handler)
  , ((aaaIncVendor, c86ControllerName), VendoredController c86Controller c86Handler)
  ]
