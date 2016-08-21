module Types where

-- These types are defined in a separate library
data Value = FloatValue Float
           | IntValue Int
           | StringValue String
           | BoolValue Bool
           | ListValue [Value]
  deriving (Show)
  
data Measurement = Measurement Value
  deriving (Show)
data Parameter = Temperature | Pressure
  deriving (Show)
  
 
-- Dummy types, should be designed later
data Property = Version | Status | SensorsList
  deriving (Show)
data Command = Command String
  deriving (Show)
type CommandResult = Either String String
data Controller = Controller String
  deriving (Show)
type SensorIndex = String
type Time = Int
type SensorInstance = (Controller, SensorIndex)
type Reading = (Time, SensorInstance, Measurement)