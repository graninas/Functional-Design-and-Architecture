module Andromeda.Hardware.Language.Hdl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.Common.Value


type Hdl = [HdlMethod]

data HdlMethod
  = SetupController DeviceName ControllerName ComponentPassport
  | RegisterComponent ControllerName ComponentIndex ComponentPassport
  | ReadSensor ControllerName ComponentIndex (Either String Measurement -> Hdl)
  | GetStatus ControllerName (Either String Status -> Hdl)
  | Report Message
  | Store Key Value
