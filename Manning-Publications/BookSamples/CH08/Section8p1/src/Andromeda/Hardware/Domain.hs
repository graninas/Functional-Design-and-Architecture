module Andromeda.Hardware.Domain where

import Andromeda.Common

newtype DeviceName = DeviceName String
  deriving (Show, Eq, Ord)

newtype ComponentIndex = ComponentIndex String
  deriving (Show, Eq, Ord)

newtype ControllerName = ControllerName String
  deriving (Show, Eq, Ord)

newtype Controller = Controller ControllerName
  deriving (Show, Eq, Ord)


type Command = String
type ParamName = String

data Param
  = Param String
  | ValueParam Value
  | UnitParam PhysicalUnit Value
  | ComponentIndexParam ComponentIndex
  deriving (Show, Eq, Ord)

data CommandResult
  = CommandSuccess [Property]
  | CommandFail String
  deriving (Show, Eq, Ord)

data ControllerStatus
  = ControllerOk
  | ControllerFail String
  deriving (Show, Eq, Ord)



data HardwareFailure
  = DeviceNotFound String
  | DevicePartNotFound String
  | NoDataFromSensor String
  | DevicePropertyNotSpecified String
  deriving (Show, Eq, Ord)
