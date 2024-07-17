module Andromeda.LogicControl.Domain where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain

type Message = String
type Key = String



data LogicControlFailure
  = HardwareFailure HardwareFailure
  | LogicControlFailure String
  deriving (Show, Eq, Ord)
