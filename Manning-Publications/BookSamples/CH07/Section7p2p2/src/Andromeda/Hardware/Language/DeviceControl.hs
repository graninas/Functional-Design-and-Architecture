{-# LANGUAGE GADTs #-}

module Andromeda.Hardware.Language.DeviceControl where

import Andromeda.Common
import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain


data DeviceControlMethod a where
  GetStatus  :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
  ReadSensor :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure SensorMeasurement)
  GetProperty :: Controller -> PropertyName -> [Param] -> DeviceControlMethod (Either HardwareFailure (Maybe Property))
  EvalCommand :: Controller -> Command -> [Param] -> DeviceControlMethod (Either HardwareFailure CommandResult)


getStatus' :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
getStatus' ctrl = GetStatus ctrl

readSensor' :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure SensorMeasurement)
readSensor' ctrl idx = ReadSensor ctrl idx

getProperty' :: Controller -> PropertyName -> [Param] -> DeviceControlMethod (Either HardwareFailure (Maybe Property))
getProperty' ctrl propName params = GetProperty ctrl propName params

evalCommand' :: Controller -> Command -> [Param] -> DeviceControlMethod (Either HardwareFailure CommandResult)
evalCommand' ctrl cmd params = EvalCommand ctrl cmd params
