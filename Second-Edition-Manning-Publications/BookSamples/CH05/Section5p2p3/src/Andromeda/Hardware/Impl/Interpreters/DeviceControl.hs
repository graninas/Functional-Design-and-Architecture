module Andromeda.Hardware.Impl.Interpreters.DeviceControl where

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import qualified Data.Map as Map






interpretDeviceControlMethod
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (RImpl.Devices -> next -> IO RImpl.Devices)
  -> L.DeviceControlMethod next
  -> IO RImpl.Devices

interpretDeviceControlMethod devices service nextInterp (L.GetStatus ctrl next) = do
  let nextScript = next $ Right T.StatusOk      -- Dummy
  nextInterp devices nextScript


interpretDeviceControlMethod devices service nextInterp (L.ReadSensor ctrl idx next) = do
  let nextScript = next $ Right $ T.Measurement T.Temperature 100.0      -- Dummy
  nextInterp devices nextScript




runDeviceControl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (RImpl.Devices -> next -> IO RImpl.Devices)
  -> L.DeviceControl next
  -> IO RImpl.Devices
runDeviceControl devices _ _ [] = pure devices
runDeviceControl devices service nextInterp (m:ms) = do
  devices' <- interpretDeviceControlMethod devices service nextInterp m
  runDeviceControl devices service nextInterp ms
