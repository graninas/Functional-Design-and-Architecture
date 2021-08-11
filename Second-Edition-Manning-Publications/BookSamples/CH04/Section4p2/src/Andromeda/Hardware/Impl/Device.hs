module Andromeda.Hardware.Impl.Device (
    makeDevice,
    blankDevice,
    getDevicePart
 ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Language.Hdl (Hdl, ComponentIndex, ComponentDef(..))
import Andromeda.Hardware.Impl.Component (VendorComponents, VendorComponent (..), SensorAPI, ControllerAPI)
import Andromeda.Hardware.Impl.Device.Types (Device(..), DevicePart (..))


import Data.Map (Map)
import qualified Data.Map as Map


blankDevice :: Device
blankDevice = Device Map.empty


-- There are different ways to provide the context
makeDevice
  :: VendorComponents
  -> Hdl
  -> IO Device
makeDevice vendorComponents hdl =
  makeDevice' vendorComponents hdl blankDevice

-- Traversing the list of components (definitions)
makeDevice' :: VendorComponents -> Hdl -> Device -> IO Device
makeDevice' vendorComponents [] device = pure device
makeDevice' vendorComponents (componentDef:cs) device = do
  let ePart = validateComponent vendorComponents componentDef
  case ePart of
    Left err   -> error err                  -- Bad practice!
    Right part -> do
      let (ComponentDef idx _) = componentDef
      let device' = addComponent idx part device
      makeDevice' vendorComponents cs device'


addComponent :: ComponentIndex -> DevicePart -> Device -> Device
addComponent idx part (Device parts) =
  Device (Map.insert idx part parts)


validateComponent
  :: VendorComponents
  -> ComponentDef
  -> Either String DevicePart
validateComponent vendorComponents componentDef = let
  (ComponentDef _ passp) = componentDef
  (ComponentPassport _ cName _ cVendor) = passp
  in case Map.lookup cName vendorComponents of
      Just vendorComponent -> Right (DevicePart vendorComponent)
      Nothing              -> Left ("Component not found: " <> cVendor <> " " <> cName)


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
