module Andromeda.Hardware.Impl.Interpreters.DeviceControl where

import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Component as CImpl

import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)


getDevice :: IORef RImpl.Devices -> T.Controller -> IO (Maybe (TImpl.ControllerImpl, TImpl.Device))
getDevice devicesRef ctrl = do
  devices <- readIORef devicesRef
  pure $ Map.lookup ctrl devices


interpretDeviceControlMethod :: RImpl.HardwareRuntime -> L.DeviceControlMethod a -> IO a

interpretDeviceControlMethod runtime (L.GetStatus ctrl next) =
  -- TODO: dummy
  pure $ next $ Right T.StatusOk

interpretDeviceControlMethod runtime (L.ReadSensor controller idx next) = do
  let RImpl.HardwareRuntime {_devicesRef, _hardwareServiceRef} = runtime

  service <- readIORef _hardwareServiceRef

  mbDevice <- getDevice _devicesRef controller
  case mbDevice of
    Nothing -> pure $ next $ Left "Device not found"
    Just (_, device) -> do
      mbDevicePart <- SImpl.getDevicePart service idx device
      case mbDevicePart of
        Nothing -> pure $ next $ Left $ "Device part not found: " <> show idx
        Just devicePart -> do
          measurement <- TImpl.withHandler devicePart $ \handler ->
            CImpl.readMeasurement handler
          pure $ next $ Right measurement

runDeviceControl :: RImpl.HardwareRuntime -> L.DeviceControl a -> IO a
runDeviceControl runtime hil = foldFree (interpretDeviceControlMethod runtime) hil
