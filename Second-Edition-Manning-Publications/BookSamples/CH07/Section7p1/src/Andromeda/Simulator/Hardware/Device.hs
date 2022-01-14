module Andromeda.Simulator.Hardware.Device where

import Andromeda.Hardware

import qualified Data.Map as Map

import Control.Concurrent (ThreadId)


data DevicePartSim = DevicePartSim
  { devicePartSimThreadId :: ThreadId
  , devicePartSimDef :: ComponentPassport
  }


data ControllerSim = ControllerSim
  { ctrlSimThreadId :: ThreadId
  , ctrlSimDef :: (ControllerName, ComponentPassport)
  , ctrlSimDevicePartsRef :: IORef (Map ComponentIndex DevicePartSim)
  }


controllerWorker :: IO ThreadId
controllerWorker = forever (pure ())

sensorWorker :: Parameter -> IO ThreadId
sensorWorker _ = forever (pure ())
