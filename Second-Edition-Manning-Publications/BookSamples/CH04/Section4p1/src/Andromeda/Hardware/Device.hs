module Andromeda.Hardware.Device (
    Device,
    DeviceComponent (..),
    WithHandler (..),
    withHandler,
    makeDevice,
    blankDevice,
    addSensor,
    addController,
    getComponent,
    updateComponent
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Components.API (SensorAPI, ControllerAPI)
import Andromeda.Hardware.Hdl

import Data.Map (Map)
import qualified Data.Map as Map

-- Design flaw: is it an implementation detail or a public definition?
data DeviceComponent
  = SensorImpl     ComponentDef SensorAPI
  | ControllerImpl ComponentDef ControllerAPI


data Device
  = DeviceImpl (Map ComponentIndex DeviceComponent)


blankDevice :: Device
blankDevice = DeviceImpl Map.empty


makeDevice
  :: VendorComponents
  -> Hdl
  -> Device
makeDevice vendorComponents hdl = makeDevice' hdl blankDevice
  where
    -- Traversing the list of devices
    makeDevice' [] d = d
    makeDevice' (c:cs) d = makeDevice' cs (add' c d)

    -- Creating a specific component (implementation)
    -- by its definition and adding into the Device type
    add' componentDef = case validateComponent vendorComponents componentDef of
      Left err -> error err                  -- Bad practice!
      Right component -> addComponent idx component d


addComponent
  :: ComponentIndex
  -> DeviceComponent
  -> Device
  -> Device
addComponent idx component (DeviceImpl components) =
  DeviceImpl (Map.insert idx component components)


validateComponent
  :: VendorComponents
  -> ComponentDef
  -> Either String DeviceComponent
validateComponent vendorComponents
  def@(ComponentDef cClass cName cGuid cVendor) =
    case Map.lookup (cVendor, cName) vendorComponents of
      Nothing -> Left ("Component not found: " <> cVendor <> " " <> cName)
      Just component -> Right component


getComponent :: ComponentIndex -> Device
             -> Maybe DeviceComponent
getComponent idx (DeviceImpl components) = Map.lookup idx components

updateComponent :: ComponentIndex -> DeviceComponent
                -> Device -> Maybe Device
updateComponent = undefined




class WithHandler handlerAPI where
  withHandler :: DeviceComponent
              -> (handlerAPI -> IO ())
              -> IO ()

instance WithHandler SensorAPI where
  withHandler (SensorImpl _ handler) f = f handler
  withHandler _ _ = error "Invalid component API handler"

instance WithHandler ControllerAPI where
  withHandler (ControllerImpl _ handler) f = f handler
  withHandler _ _ = error "Invalid component API handler"
