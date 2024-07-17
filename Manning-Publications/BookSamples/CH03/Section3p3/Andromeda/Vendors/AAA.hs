module Andromeda.Vendors.AAA where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Hdl

-- import Native AAA sensors library here or do FFI

aaaIncVendor :: Vendor
aaaIncVendor = "AAA Inc."


t25SensorName :: ComponentName
t25SensorName = "AAA-T-25"

p02SensorName :: ComponentName
p02SensorName = "AAA-З-02"

c86ControllerName :: ComponentName
c86ControllerName = "AAA-C-86"


t25Sensor :: ComponentPassport
t25Sensor =
  ComponentPassport Sensors t25SensorName "some_guid1" aaaIncVendor

p02Sensor :: ComponentPassport
p02Sensor =
  ComponentPassport Sensors p02SensorName "some_guid2" aaaIncVendor

c86Controller :: ComponentPassport
c86Controller =
  ComponentPassport Controllers c86ControllerName "some_guid3" aaaIncVendor





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
