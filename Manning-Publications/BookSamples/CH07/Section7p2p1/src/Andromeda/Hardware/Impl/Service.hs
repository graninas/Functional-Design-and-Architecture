module Andromeda.Hardware.Impl.Service
  ( HardwareService (..)
  ) where

import Andromeda.Hardware.Common (ComponentPassport)
import Andromeda.Hardware.Domain (ControllerName, ComponentIndex, DeviceName)
import Andromeda.Hardware.Language.Hdl (Hdl)
import Andromeda.Hardware.Impl.Device.Types (ControllerImpl, Device, DevicePart)


data HardwareService = HardwareService
  { makeController  :: ControllerName -> ComponentPassport -> IO (Either String ControllerImpl)
  , makeBlankDevice :: DeviceName -> ControllerImpl -> IO Device
  , makeDevicePart  :: ComponentPassport -> IO (Either String DevicePart)
  , addDevicePart   :: ComponentIndex -> DevicePart -> Device -> IO ()
  , getDevicePart   :: ComponentIndex -> Device -> IO (Maybe DevicePart)
  }
