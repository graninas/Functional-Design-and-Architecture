{-# LANGUAGE GADTs #-}

module Andromeda.Hardware.Impl.Interpreters.DeviceControl where

import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device
import Andromeda.Simulator.Runtime

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Monad.Free (foldFree)


interpretDeviceControlMethod :: RImpl.HardwareRuntime -> L.DeviceControlMethod a -> IO a

interpretDeviceControlMethod runtime (L.GetStatus ctrl) = do
  let SimulatorRuntime{_controllerSimsVar} = runtime
  ctrlSims <- takeMVar _controllerSimsVar

  let tryGetStatus = case Map.lookup ctrl ctrlSims of
        Nothing -> pure $ Left $ DeviceNotFound $ show ctrl
        Just ControllerSim{ctrlSimRequestVar} -> do
          statusResponseVar <- newEmptyMVar
          putMVar ctrlSimRequestVar $ GetControlerSimStatus statusResponseVar
          ctrlStatus <- takeMVar statusResponseVar
          pure $ Right ctrlStatus

  eStatus <- tryGetStatus
  putMVar _controllerSimsVar ctrlSims
  pure $ next eStatus

interpretDeviceControlMethod runtime (L.ReadSensor ctrl idx) = do
  let SimulatorRuntime{_controllerSimsVar} = runtime
  ctrlSims <- takeMVar _controllerSimsVar

  let tryGetStatus = case Map.lookup ctrl ctrlSims of
        Nothing -> pure $ Left $ DeviceNotFound $ show ctrl
        Just ControllerSim{ctrlSimRequestVar} -> do
          statusResponseVar <- newEmptyMVar
          putMVar ctrlSimRequestVar $ ReadSimSensor idx statusResponseVar
          ctrlStatus <- takeMVar statusResponseVar
          pure $ Right ctrlStatus

  eStatus <- tryGetStatus
  putMVar _controllerSimsVar ctrlSims
  pure $ next eStatus
