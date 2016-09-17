module Andromeda.Hardware.Parameter where

data Parameter = Temperature | Pressure
  deriving (Read, Show)

-- Dummy type. Should be designed later.
type Measurement = ()

toMeasurement :: Parameter -> Measurement
toMeasurement _ = ()

temperature = Temperature
pressure = Pressure

