module Listing2 where

data SensorType
  = TemperatureSensor
  | PressureSensor
  | PropellantDepletionSensor

data DeviceType
  = Sensor SensorType
  | Controller
  | Booster
  | RotaryEngine
  | FuelTank

type Name = String

type Components = [Device]

data Device = Device Name DeviceType Components
