module Andromeda.Hardware.Domain where


newtype DeviceName = DeviceName String
  deriving (Show, Eq, Ord)

newtype ComponentIndex = ComponentIndex String
  deriving (Show, Eq, Ord)

newtype ControllerName = ControllerName String
  deriving (Show, Eq, Ord)
  
newtype Controller = Controller ControllerName
  deriving (Show, Eq, Ord)



data Status
  = StatusOk
  | StatusFail String
  deriving (Show, Eq, Ord)
