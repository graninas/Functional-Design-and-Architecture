module Andromeda.Hardware.Domain where


type DeviceName = String
type ComponentIndex = String
type ControllerName = String
newtype Controller = Controller ControllerName
  deriving (Show, Eq, Ord)



data Status
  = StatusOk
  | StatusFail String
  deriving (Show, Eq, Ord)
