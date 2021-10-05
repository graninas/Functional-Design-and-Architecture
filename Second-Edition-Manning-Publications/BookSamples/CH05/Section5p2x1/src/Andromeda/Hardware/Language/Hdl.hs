module Andromeda.Hardware.Language.Hdl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.Common.Value


type Hdl = [HdlMethod]

data HdlMethod
  = SetupController DeviceName ControllerName ComponentPassport (Controller -> Hdl)
  | RegisterComponent Controller ComponentIndex ComponentPassport
  | ReadSensor Controller ComponentIndex (Either String Measurement -> Hdl)
  | GetStatus Controller (Either String Status -> Hdl)
  | Report Message
