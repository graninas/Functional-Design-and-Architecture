module Andromeda.Assets.Vendors.AAA where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Vendors.Components
import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Hdl

import qualified Data.Map as Map

-- import Native AAA sensors library here or do FFI

aaaInc :: Vendor
aaaInc = "AAA Inc."


aaaTemperature25Name :: ComponentName
aaaTemperature25Name = "AAA-T-25"

aaaPressure02Name :: ComponentName
aaaPressure02Name = "AAA-З-02"

aaaController86Name :: ComponentName
aaaController86Name = "AAA-C-86"


guid1 = "some_guid1"
guid2 = "some_guid2"
guid3 = "some_guid3"


aaaTemperature25Passport = ComponentPassport Sensors     aaaTemperature25Name guid2 aaaInc
aaaPressure02Passport    = ComponentPassport Sensors     aaaPressure02Name    guid1 aaaInc
aaaController86Passport  = ComponentPassport Controllers aaaController86Name  guid3 aaaInc




aaaTemperature25Handler :: SensorAPI
aaaTemperature25Handler = SensorAPI
  { reset = putStrLn $ aaaTemperature25Name <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ aaaTemperature25Name <> " callback."
  }

aaaPressure02Handler :: SensorAPI
aaaPressure02Handler = SensorAPI
  { reset = putStrLn $ aaaPressure02Name <> " reset."
  , readMeasurement = pure $ Measurement Temperature 100.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ aaaPressure02Name <> " callback."
  }

aaaController86Handler :: ControllerAPI
aaaController86Handler = ControllerAPI
  { reboot = putStrLn $ aaaController86Name <> " reset."
  , turnOff = putStrLn $ aaaController86Name <> " turn off."
  , eval = \cmd -> putStrLn $ aaaController86Name <> " eval cmd: " <> cmd
  , doSomethingElse = putStrLn $ aaaController86Name <> " do something else"
  }



aaaVendorComponents :: VendorComponents
aaaVendorComponents = Map.fromList
  [ (aaaTemperature25Name, VendoredSensor     aaaTemperature25Passport aaaTemperature25Handler)
  , (aaaPressure02Name,    VendoredSensor     aaaPressure02Passport    aaaPressure02Handler)
  , (aaaController86Name,  VendoredController aaaController86Passport  aaaController86Handler)
  ]
