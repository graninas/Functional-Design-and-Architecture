module Andromeda.Hardware.Language.DeviceControl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain


type DeviceControl next = [DeviceControlMethod next]

data DeviceControlMethod next
  = GetStatus Controller (Either String Status -> next)
  | ReadSensor Controller ComponentIndex (Either String Measurement -> next)
