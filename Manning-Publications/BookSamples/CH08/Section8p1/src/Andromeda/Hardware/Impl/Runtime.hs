module Andromeda.Hardware.Impl.Runtime where

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl


import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)


type DeviceImpl = (TImpl.ControllerImpl, TImpl.Device)
type Devices = Map.Map T.Controller DeviceImpl


data HardwareRuntime = HardwareRuntime
  { _devicesRef :: IORef Devices
  , _hardwareServiceRef :: IORef SImpl.HardwareService
  }


createHardwareRuntime :: SImpl.HardwareService -> IO HardwareRuntime
createHardwareRuntime hService = do
  devicesRef <- newIORef Map.empty
  hServiceRef <- newIORef hService
  pure $ HardwareRuntime devicesRef hServiceRef
