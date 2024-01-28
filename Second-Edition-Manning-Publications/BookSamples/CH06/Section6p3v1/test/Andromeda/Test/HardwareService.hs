module Andromeda.Test.HardwareService where

import Andromeda.Hardware

import Andromeda.TestData.Components (thermometer1Passp, thermometer1Handler,
  pressure1Passp, pressure1Handler)

mockedThermometer1 :: DevicePart
mockedThermometer1 = DevicePart (VendoredSensor thermometer1Passp thermometer1Handler)

mockedPressure1 :: DevicePart
mockedPressure1 = DevicePart (VendoredSensor pressure1Passp pressure1Handler)
