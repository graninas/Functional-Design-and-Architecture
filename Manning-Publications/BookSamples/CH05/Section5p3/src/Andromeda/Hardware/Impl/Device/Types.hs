module Andromeda.Hardware.Impl.Device.Types (
    ControllerImpl(..),
    Device (..),
    DevicePart (..),
    WithHandler (..),
    withHandler,
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.Hardware.Language.Hdl
import Andromeda.Hardware.Impl.Component (VendorComponents, VendorComponent (..), SensorAPI, ControllerAPI)

import qualified Data.Map as Map

data ControllerImpl = ControllerImpl ControllerName VendorComponent
data DevicePart = DevicePart VendorComponent {- some state here -}
data Device = Device DeviceName ControllerImpl (Map.Map ComponentIndex DevicePart)


class WithHandler handlerAPI where
  withHandler :: DevicePart
              -> (handlerAPI -> IO a)
              -> IO a

instance WithHandler SensorAPI where
  withHandler (DevicePart (VendoredSensor _ handler)) f = f handler
  withHandler _ _ = error "Invalid part API handler"

instance WithHandler ControllerAPI where
  withHandler (DevicePart (VendoredController _ handler)) f = f handler
  withHandler _ _ = error "Invalid part API handler"
