module Andromeda.Vendors.AAA where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Hdl

-- import Native AAA sensors library here or do FFI


aaaIncVendor = "AAA Inc."


t25SensorName :: ComponentName
t25SensorName = "AAA-T-25"

p02SensorName :: ComponentName
p02SensorName = "AAA-З-02"


t25Sensor :: ComponentDef
t25Sensor =
  ComponentDef Sensors t25SensorName "some_guid1" aaaIncVendor

p02Sensor :: ComponentDef
p02Sensor =
  ComponentDef Sensors p02SensorName "some_guid2" aaaIncVendor





t25Handler :: SensorAPI
t25Handler = SensorAPI
  { reset = putStrLn $ t25SensorName <> " " <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ t25SensorName <> " " <> " callback."
  }

p02Handler :: SensorAPI
p02Handler = SensorAPI
  { reset = putStrLn $ p02SensorName <> " " <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ p02SensorName <> " " <> " callback."
  }
