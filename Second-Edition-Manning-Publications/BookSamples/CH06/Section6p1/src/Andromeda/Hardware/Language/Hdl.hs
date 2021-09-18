module Andromeda.Hardware.Language.Hdl where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain

import Control.Monad.Free (Free (..), liftF)


data HdlMethod next
  = SetupComponent ComponentPassport (Component -> next)
  | SetupController ControllerName ComponentPassport (Controller -> next)
  | RegisterComponent Controller ComponentIndex Component (() -> next)

instance Functor HdlMethod where
  fmap f (SetupComponent passp next) = SetupComponent passp (f . next)
  fmap f (SetupController name passp next) = SetupController name passp (f . next)
  fmap f (RegisterComponent controller cIdx component next) = RegisterComponent controller cIdx component (f . next)


type Hdl a = Free HdlMethod a



setupComponent :: ComponentPassport -> Hdl Component
setupComponent passp = liftF $ SetupComponent passp id

setupController :: ControllerName -> ComponentPassport -> Hdl Controller
setupController name passp = liftF $ SetupController name passp id

registerComponent :: Controller -> ComponentIndex -> Component -> Hdl ()
registerComponent controller cIdx component = liftF $ RegisterComponent controller cIdx component id
