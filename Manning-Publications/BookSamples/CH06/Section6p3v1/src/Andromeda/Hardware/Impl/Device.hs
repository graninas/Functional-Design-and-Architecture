module Andromeda.Hardware.Impl.Device
  ( makeBlankDevice
  , makeDevicePart
  , makeController
  , addDevicePart
  , getDevicePart
  ) where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain (ControllerName, ComponentIndex, DeviceName)
import Andromeda.Hardware.Language.Hdl (Hdl)
import Andromeda.Hardware.Impl.Component (VendorComponents, VendorComponent (..), SensorAPI, ControllerAPI)
import Andromeda.Hardware.Impl.Device.Types (ControllerImpl(..), Device(..), DevicePart (..))


import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef


makeBlankDevice :: DeviceName -> ControllerImpl -> IO Device
makeBlankDevice name ctrlImpl = do
  partsRef <- newIORef Map.empty
  pure $ Device name ctrlImpl partsRef

makeDevicePart
  :: VendorComponents
  -> ComponentPassport
  -> IO (Either String DevicePart)
makeDevicePart vendorComponents (ComponentPassport (Sensors _) cName _ cVendor) =
  pure $ case Map.lookup cName vendorComponents of
    Just vendorComponent -> Right (DevicePart vendorComponent)
    Nothing              -> Left ("Component not found: " <> cVendor <> " " <> cName)
makeDevicePart _ _ = pure $ Left "Invalid/unknown component class for a device part"

makeController
  :: VendorComponents
  -> ControllerName
  -> ComponentPassport
  -> IO (Either String ControllerImpl)
makeController vendorComponents ctrlName (ComponentPassport Controllers cName _ cVendor) =
  pure $ case Map.lookup cName vendorComponents of
    Just vendorComponent -> Right (ControllerImpl ctrlName vendorComponent)
    Nothing              -> Left ("Component not found: " <> cVendor <> " " <> cName)
makeController _ _ _ = pure $ Left "Invalid/unknown component class for a controller"


addDevicePart :: ComponentIndex -> DevicePart -> Device -> IO ()
addDevicePart idx part (Device name _ partsRef) = do
  parts <- readIORef partsRef
  writeIORef partsRef (Map.insert idx part parts)

getDevicePart :: ComponentIndex -> Device
              -> IO (Maybe DevicePart)
getDevicePart idx (Device _ _ partsRef) = do
  parts <- readIORef partsRef
  pure $ Map.lookup idx parts
