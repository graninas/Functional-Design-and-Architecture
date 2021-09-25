module Andromeda.Assets.Vendors.AAA.HardwareService where

import Andromeda.Hardware (HardwareService(..))
import qualified Andromeda.Hardware.Impl.Device as Impl
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)

aaaHardwareService :: HardwareService
aaaHardwareService = HardwareService
  { makeController = Impl.makeController aaaVendorComponents
  , makeDevicePart = Impl.makeDevicePart aaaVendorComponents
  , makeBlankDevice = Impl.makeBlankDevice
  , addDevicePart = Impl.addDevicePart
  , getDevicePart = Impl.getDevicePart
  }
