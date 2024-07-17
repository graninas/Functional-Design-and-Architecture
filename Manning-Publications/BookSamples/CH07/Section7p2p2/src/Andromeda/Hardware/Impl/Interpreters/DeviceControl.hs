{-# LANGUAGE GADTs #-}

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

interpretDeviceControlMethod runtime (L.GetStatus ctrl) =
  -- TODO: dummy
  pure $ Right T.ControllerOk

interpretDeviceControlMethod runtime (L.ReadSensor ctrl idx) = do
  let RImpl.HardwareRuntime {_devicesRef, _hardwareServiceRef} = runtime

  service <- readIORef _hardwareServiceRef

  mbDevice <- getDevice _devicesRef ctrl
  case mbDevice of
    Nothing -> pure $ Left $ T.DeviceNotFound $ show ctrl
    Just (_, device) -> do
      mbDevicePart <- SImpl.getDevicePart service idx device
      case mbDevicePart of
        Nothing -> pure $ Left $ T.DevicePartNotFound $ show idx
        Just devicePart -> do
          measurement <- TImpl.withHandler devicePart $ \handler ->
            CImpl.readMeasurement handler
          pure $ Right measurement

-- Dummy method
interpretDeviceControlMethod runtime (L.GetProperty ctrl propName params) = do
  pure $ Left $ T.DevicePropertyNotSpecified propName
-- Dummy method
interpretDeviceControlMethod runtime (L.EvalCommand ctrl cmd params) = do
  pure $ Right $ T.CommandSuccess []
