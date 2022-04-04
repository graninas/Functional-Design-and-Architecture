{-# LANGUAGE GADTs #-}

module Andromeda.Simulator.Hardware.Interpreters.DeviceControl where

import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device
import Andromeda.Simulator.Runtime

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Monad.Free (foldFree)


interpretDeviceControlMethod :: SimulatorRuntime -> L.DeviceControlMethod a -> IO a

interpretDeviceControlMethod runtime (L.GetStatus ctrl) = do
  let SimulatorRuntime{simRtControllerSimsVar} = runtime
  ctrlSims <- readMVar simRtControllerSimsVar

  let tryGetStatus = case Map.lookup ctrl ctrlSims of
        Nothing -> pure $ Left $ T.DeviceNotFound $ show ctrl
        Just ControllerSim{ctrlSimRequestVar} -> do
          statusResponseVar <- newEmptyMVar
          putMVar ctrlSimRequestVar $ GetControlerSimStatus statusResponseVar
          ctrlStatus <- takeMVar statusResponseVar
          pure $ Right ctrlStatus

  eStatus <- tryGetStatus
  pure eStatus

interpretDeviceControlMethod runtime (L.ReadSensor ctrl idx) = do
  let SimulatorRuntime{simRtControllerSimsVar} = runtime
  ctrlSims <- readMVar simRtControllerSimsVar

  let tryReadSensor = case Map.lookup ctrl ctrlSims of
        Nothing -> pure $ Left $ T.DeviceNotFound $ show ctrl
        Just ControllerSim{ctrlSimRequestVar} -> do
          mbMeasurementVar <- newEmptyMVar
          putMVar ctrlSimRequestVar $ ReadSimSensor idx mbMeasurementVar
          mbMeasurement <- takeMVar mbMeasurementVar
          pure $ case mbMeasurement of
            Nothing -> Left $ T.NoDataFromSensor $ show (ctrl, idx)
            Just m -> Right m

  eMeasurement <- tryReadSensor
  pure eMeasurement
