module Andromeda.Hardware.Impl.Runtime where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain

import Andromeda.Hardware.Impl.Device.Types

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef)


type Devices = Map.Map Controller Device


data HardwareRuntime = HardwareRuntime
  { devicesRef :: IORef Devices


  }


createHardwareRuntime :: IO HardwareRuntime
createHardwareRuntime = do
  devicesRef <- newIORef Map.empty
  pure $ HardwareRuntime devicesRef
