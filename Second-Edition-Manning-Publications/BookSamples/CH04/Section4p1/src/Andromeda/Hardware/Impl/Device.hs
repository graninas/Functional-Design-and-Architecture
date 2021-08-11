module Andromeda.Hardware.Impl.Device (
    Device,
    DevicePart (..),
    WithHandler (..),
    withHandler,
    makeDevice,
    blankDevice,
    getDevicePart
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Language.Hdl
import Andromeda.Hardware.Impl.Component (VendorComponents, VendorComponent (..), SensorAPI, ControllerAPI)

import Data.Map (Map)
import qualified Data.Map as Map


data DevicePart = DevicePart VendorComponent {- some state here -}

data Device = Device (Map ComponentIndex DevicePart)


blankDevice :: Device
blankDevice = Device Map.empty



-- There are different ways to provide the context
makeDevice
  :: VendorComponents
  -> Hdl
  -> Device
makeDevice vendorComponents hdl = makeDevice' hdl blankDevice
  where
    -- Traversing the list of components (definitions)
    makeDevice' [] device = device
    makeDevice' (c:cs) device = makeDevice' cs (add' c device)

    -- Creating a specific device part (implementation)
    -- by its definition and adding into the Device type
    add' (ComponentDef idx passp) device =
      case validateComponent vendorComponents passp of
        Left err -> error err                  -- Bad practice!
        Right part -> addComponent idx part device


addComponent
  :: ComponentIndex
  -> DevicePart
  -> Device
  -> Device
addComponent idx part (Device parts) =
  Device (Map.insert idx part parts)


validateComponent
  :: VendorComponents
  -> ComponentPassport
  -> Either String DevicePart
validateComponent vendorComponents
  (ComponentPassport _ cName _ cVendor) =
    case Map.lookup cName vendorComponents of
      Nothing -> Left ("Component not found: " <> cVendor <> " " <> cName)
      Just vendorComponent -> Right (DevicePart vendorComponent)


getDevicePart :: ComponentIndex -> Device
              -> Maybe DevicePart
getDevicePart idx (Device parts) = Map.lookup idx parts


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
