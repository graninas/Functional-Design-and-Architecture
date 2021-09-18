module Andromeda.Hardware.Impl.Service
  ( HardwareService (..)
  ) where


import Andromeda.Hardware.Language.Hdl (Hdl, ComponentIndex)
import Andromeda.Hardware.Impl.Device.Types (Device, DevicePart)

data HardwareService = HardwareService
  { makeDevice :: IO Device
  , addDevicePart :: ComponentIndex -> DevicePart ->  Device -> IO ()
  , getDevicePart  :: ComponentIndex -> Device -> IO (Maybe DevicePart)
  }
