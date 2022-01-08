module Andromeda.Hardware.Domain where


newtype DeviceName = DeviceName String
  deriving (Show, Eq, Ord)

newtype ComponentIndex = ComponentIndex String
  deriving (Show, Eq, Ord)

newtype ControllerName = ControllerName String
  deriving (Show, Eq, Ord)

newtype Controller = Controller ControllerName
  deriving (Show, Eq, Ord)




data ControllerStatus
  = ControllerOk
  | ControllerFail String
  deriving (Show, Eq, Ord)



data HardwareFailure
  = DeviceNotFound String
  | DevicePartNotFound String
  deriving (Show, Eq, Ord)
