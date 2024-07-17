module Andromeda.Assets.Vendors.BBB.ComponentsAPI where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.Assets.Vendors.BBB.Common
import Andromeda.Assets.Vendors.BBB.Components
import Andromeda.Hardware.Impl.Component

import qualified Data.Map as Map

bbbTemperature25Handler :: SensorAPI
bbbTemperature25Handler = SensorAPI
  { reset = putStrLn $ bbbTemperature25Name <> " reset."
  , readMeasurement = pure $ Measurement Temperature 25.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ bbbTemperature25Name <> " callback."
  }

bbbPressure02Handler :: SensorAPI
bbbPressure02Handler = SensorAPI
  { reset = putStrLn $ bbbPressure02Name <> " reset."
  , readMeasurement = pure $ Measurement Pressure 25.0   -- dummy
  , setCallback = \_ _ -> putStrLn $ bbbPressure02Name <> " callback."
  }

bbbController86Handler :: ControllerAPI
bbbController86Handler = ControllerAPI
  { reboot = putStrLn $ bbbController86Name <> " reset."
  , turnOff = putStrLn $ bbbController86Name <> " turn off."
  , eval = \cmd -> putStrLn $ bbbController86Name <> " eval cmd: " <> cmd
  , getStatus = pure StatusOk
  }



bbbVendorComponents :: VendorComponents
bbbVendorComponents = Map.fromList
  [ (bbbTemperature25Name, VendoredSensor     bbbTemperature25Passport bbbTemperature25Handler)
  , (bbbPressure02Name,    VendoredSensor     bbbPressure02Passport    bbbPressure02Handler)
  , (bbbController86Name,  VendoredController bbbController86Passport  bbbController86Handler)
  ]
