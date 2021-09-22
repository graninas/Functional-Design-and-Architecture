module Andromeda.Hardware.Impl.DeviceControlInterpreter where

import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Component as CImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)


getDevice :: RImpl.HardwareRuntime -> T.Controller -> IO (Maybe TImpl.Device)
getDevice RImpl.HardwareRuntime {devicesRef} ctrl = do
  devices <- readIORef devicesRef
  pure $ Map.lookup ctrl devices


interpretDeviceControlMethod :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.DeviceControlMethod a -> IO a

interpretDeviceControlMethod runtime service (L.GetStatus ctrl next) =
  -- TODO: dummy
  pure $ next $ Right T.StatusOk

interpretDeviceControlMethod runtime service (L.ReadSensor controller idx next) = do
  mbDevice <- getDevice runtime controller
  case mbDevice of
    Nothing -> pure $ next $ Left "Device not found"
    Just device -> do
      mbDevicePart <- SImpl.getDevicePart service idx device
      case mbDevicePart of
        Nothing -> pure $ next $ Left $ "Device part not found: " <> idx
        Just devicePart -> do
          measurement <- TImpl.withHandler devicePart $ \handler ->
            CImpl.readMeasurement handler
          pure $ next $ Right measurement

runDeviceControl :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.DeviceControl a -> IO a
runDeviceControl runtime service hil = foldFree (interpretDeviceControlMethod runtime service) hil
