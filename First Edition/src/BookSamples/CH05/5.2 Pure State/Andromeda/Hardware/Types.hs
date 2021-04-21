module Andromeda.Hardware.Types where

-- These types are defined in a separate library
data Value = FloatValue Float
           | IntValue Int
           | StringValue String
           | BoolValue Bool
           | ListValue [Value]
  deriving (Show, Read, Eq)
  
data Measurement = Measurement Value
  deriving (Show, Read, Eq)

-- Dummy type
data Parameter = Temperature | Pressure
  deriving (Show, Read, Eq)

temperature = Temperature
pressure = Pressure

toMeasurement p = undefined