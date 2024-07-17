module Andromeda.Hardware.Impl.Device.Types (
    DeviceName,
    Device (..),
    DevicePart (..),
    WithHandler (..),
    withHandler,
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Language.Hdl
import Andromeda.Hardware.Impl.Component (VendorComponents, VendorComponent (..), SensorAPI, ControllerAPI)

import Data.Map (Map)
import qualified Data.Map as Map


type DeviceName = String
data DevicePart = DevicePart VendorComponent {- some state here -}
data Device = Device DeviceName (Map ComponentIndex DevicePart)


class WithHandler handlerAPI where
  withHandler :: DevicePart
              -> (handlerAPI -> IO ())
              -> IO ()

instance WithHandler SensorAPI where
  withHandler (DevicePart (VendoredSensor _ handler)) f = f handler
  withHandler _ _ = error "Invalid part API handler"

instance WithHandler ControllerAPI where
  withHandler (DevicePart (VendoredController _ handler)) f = f handler
  withHandler _ _ = error "Invalid part API handler"
