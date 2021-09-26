
module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common.Value

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L


data LogicControlMethod
  = EvalHdl (L.Hdl LogicControl)
  | EvalDeviceControl (L.DeviceControl LogicControl)
  | Report Message
  | Store Key Value


type LogicControl = [LogicControlMethod]
