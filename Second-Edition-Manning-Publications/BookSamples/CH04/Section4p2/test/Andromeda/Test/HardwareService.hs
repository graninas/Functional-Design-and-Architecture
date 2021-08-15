module Andromeda.Test.HardwareService where

import Andromeda.Hardware
import qualified Andromeda.Hardware.Impl.Device.Types as D

import Andromeda.TestData.Components (thermometer1Passp, thermometer1Handler)

import qualified Data.Map as Map

mockedDevicePart :: D.DevicePart
mockedDevicePart = D.DevicePart (VendoredSensor thermometer1Passp thermometer1Handler)

mockedHardwareService :: HardwareService
mockedHardwareService = HardwareService
  { makeDevice     = \_ -> pure (D.Device "mocked" Map.empty)
  , getBlankDevice = error "getBlankDevice not supported"
  , getDevicePart  = \idx device -> case (idx, device) of
    ("t1", D.Device "mocked" _) -> pure (Just mockedDevicePart)
    _ -> error "Unknown device"
  }
