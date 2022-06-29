module Andromeda.Simulator.Hardware.Device where

import Andromeda.Hardware

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId)



-- Old Req-Resp pattern
-- data ControllerSimRequest
--   = GetControlerSimStatus
--
-- data ControllerSimResponse
--   = ControlerSimStatusOk

-- type ControllerSimChannel = (MVar ControllerSimRequest, MVar ControllerSimResponse)
-- type DevicePartSimChannel = (MVar DevicePartSimRequest, MVar DevicePartSimResponse)

-- Improved Req-Resp pattern

data ControllerSimRequest
  = GetControlerSimStatus (MVar ControllerStatus)
  | SetControlerSimStatus ControllerStatus
  | ReadSimSensor ComponentIndex (MVar (Maybe SensorMeasurement))

data DummyDevicePartSimStatus = DummyDevicePartSimStatus

data DevicePartSimRequest
  = SetSensorSimRange (Int, Int)
  | ProduceMeasurement (MVar (Maybe SensorMeasurement))

data DevicePartSim = DevicePartSim
  { devicePartSimThreadId   :: ThreadId
  , devicePartSimDef        :: ComponentPassport
  , devicePartSimRequestVar :: MVar DevicePartSimRequest
  }


type DevicePartSims = Map.Map ComponentIndex DevicePartSim

data ControllerSim = ControllerSim
  { ctrlSimThreadId       :: ThreadId
  , ctrlSimDef            :: (ControllerName, ComponentPassport)
  , ctrlSimDevicePartsVar :: MVar DevicePartSims
  , ctrlSimRequestVar     :: MVar ControllerSimRequest
  }
