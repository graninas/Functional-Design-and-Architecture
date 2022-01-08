{-# LANGUAGE GADTs #-}

module Andromeda.Hardware.Language.DeviceControl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain


data DeviceControlMethod a where
  GetStatus  :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
  ReadSensor :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure Measurement)

getStatus' :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
getStatus' controller = GetStatus controller

readSensor' :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure Measurement)
readSensor' controller idx = ReadSensor controller idx
