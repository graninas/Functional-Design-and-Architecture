module Andromeda.Hardware.Impl.Interpreters.DeviceControl where

import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

import qualified Data.Map as Map



interpretDeviceControlMethod
  :: RImpl.Runtime
  -> (RImpl.Runtime -> next -> IO RImpl.Runtime)
  -> L.DeviceControlMethod next
  -> IO RImpl.Runtime

interpretDeviceControlMethod runtime nextInterp (L.GetStatus ctrl next) = do
  let nextScript = next $ Right T.StatusOk      -- Dummy
  nextInterp runtime nextScript


interpretDeviceControlMethod runtime nextInterp (L.ReadSensor ctrl idx next) = do
  let nextScript = next $ Right $ T.Measurement T.Temperature 100.0      -- Dummy
  nextInterp runtime nextScript



runDeviceControl
  :: RImpl.Runtime
  -> (RImpl.Runtime -> next -> IO RImpl.Runtime)
  -> L.DeviceControl next
  -> IO RImpl.Runtime
runDeviceControl runtime _ [] = pure runtime
runDeviceControl runtime nextInterp (m:ms) = do
  runtime' <- interpretDeviceControlMethod runtime nextInterp m
  runDeviceControl runtime' nextInterp ms
