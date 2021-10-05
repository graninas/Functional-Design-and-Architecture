module Andromeda.Hardware.Language.Hdl where


import Andromeda.Hardware.Common (ComponentPassport)
import Andromeda.Hardware.Domain (DeviceName, ControllerName, ComponentIndex, Controller)


type Hdl next = [HdlMethod next]

data HdlMethod next
  = SetupController DeviceName ControllerName ComponentPassport (Controller -> next)
  | RegisterComponent Controller ComponentIndex ComponentPassport
