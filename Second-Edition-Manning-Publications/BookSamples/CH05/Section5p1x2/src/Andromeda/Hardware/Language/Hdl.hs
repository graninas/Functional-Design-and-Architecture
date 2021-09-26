module Andromeda.Hardware.Language.Hdl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.Common.Value



type Hdl next = [HdlMethod next]

data HdlMethod next
  = SetupController DeviceName ControllerName ComponentPassport (Controller -> next)
  | RegisterComponent Controller ComponentIndex ComponentPassport
