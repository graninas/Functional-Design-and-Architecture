module Andromeda.Hardware.Device (
    Device,
    DeviceComponent (..),
    WithHandler (..),
    withHandler,
    makeDevice,
    blankDevice,
    getComponent,
    updateComponent
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Vendors.Components
import Andromeda.Hardware.Components.API
import Andromeda.Hardware.Hdl

import Data.Map (Map)
import qualified Data.Map as Map


data DeviceComponent = DeviceComponent VendorComponent {- some state here -}

data Device = Device (Map ComponentIndex DeviceComponent)


blankDevice :: Device
blankDevice = Device Map.empty



-- There are different ways to provide the context
makeDevice
  :: VendorComponents
  -> Hdl
  -> Device
makeDevice vendorComponents hdl = makeDevice' hdl blankDevice
  where
    -- Traversing the list of devices
    makeDevice' [] device = device
    makeDevice' (c:cs) device = makeDevice' cs (add' c device)

    -- Creating a specific component (implementation)
    -- by its definition and adding into the Device type
    add' (ComponentDef idx passp) device = case validateComponent vendorComponents passp of
      Left err -> error err                  -- Bad practice!
      Right component -> addComponent idx component device


addComponent
  :: ComponentIndex
  -> DeviceComponent
  -> Device
  -> Device
addComponent idx component (Device components) =
  Device (Map.insert idx component components)


validateComponent
  :: VendorComponents
  -> ComponentPassport
  -> Either String DeviceComponent
validateComponent vendorComponents
  def@(ComponentPassport _ cName _ cVendor) =
    case Map.lookup cName vendorComponents of
      Nothing -> Left ("Component not found: " <> cVendor <> " " <> cName)
      Just vendorComponent -> Right (DeviceComponent vendorComponent)


getComponent :: ComponentIndex -> Device
             -> Maybe DeviceComponent
getComponent idx (Device components) = Map.lookup idx components

updateComponent :: ComponentIndex -> DeviceComponent
                -> Device -> Maybe Device
updateComponent = undefined




class WithHandler handlerAPI where
  withHandler :: DeviceComponent
              -> (handlerAPI -> IO ())
              -> IO ()

instance WithHandler SensorAPI where
  withHandler (DeviceComponent (VendoredSensor _ handler)) f = f handler
  withHandler _ _ = error "Invalid component API handler"

instance WithHandler ControllerAPI where
  withHandler (DeviceComponent (VendoredController _ handler)) f = f handler
  withHandler _ _ = error "Invalid component API handler"
