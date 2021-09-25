module Andromeda.Hardware.Domain where


type DeviceName = String
type ComponentIndex = String
type ControllerName = String
data Controller = Controller ControllerName
  deriving (Show, Eq, Ord)



data Status
  = StatusOk
  deriving (Show, Eq, Ord)

type Message = String
type Key = String
