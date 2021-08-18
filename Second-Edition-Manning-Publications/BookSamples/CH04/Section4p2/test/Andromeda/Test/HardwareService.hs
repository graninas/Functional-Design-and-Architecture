module Andromeda.Test.HardwareService where

import Andromeda.Hardware

import Andromeda.TestData.Components (thermometer1Passp, thermometer1Handler,
  pressure1Passp, pressure1Handler)

mockedThermometer1 :: DevicePart
mockedThermometer1 = DevicePart (VendoredSensor thermometer1Passp thermometer1Handler)

mockedPressure1 :: DevicePart
mockedPressure1 = DevicePart (VendoredSensor pressure1Passp pressure1Handler)

mockedHardwareService :: HardwareService
mockedHardwareService = HardwareService
  { makeDevice     = mockedMakeDevice "mocked"
  , getBlankDevice = error "getBlankDevice not supported"
  , getDevicePart  = mockedGetDevicePart
  }

mockedMakeDevice :: DeviceName -> Hdl -> IO Device
mockedMakeDevice name _ = pure (Device name mempty)

mockedGetDevicePart
  :: ComponentIndex
  -> Device
  -> IO (Maybe DevicePart)
mockedGetDevicePart idx device =
  case (idx, device) of
    ("t1", Device "mocked" _) -> pure (Just mockedThermometer1)
    ("p1", Device "mocked" _) -> pure (Just mockedPressure1)
    _ -> pure Nothing
