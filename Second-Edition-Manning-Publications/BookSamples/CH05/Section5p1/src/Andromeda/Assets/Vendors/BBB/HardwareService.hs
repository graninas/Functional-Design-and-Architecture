module Andromeda.Assets.Vendors.BBB.HardwareService where

import Andromeda.Hardware (HardwareService(..))
import qualified Andromeda.Hardware.Impl.Device as Impl
import Andromeda.Assets.Vendors.BBB.ComponentsAPI (bbbVendorComponents)


bbbHardwareService :: HardwareService
bbbHardwareService = HardwareService
  { makeController = Impl.makeController bbbVendorComponents
  , makeDevicePart = Impl.makeDevicePart bbbVendorComponents
  , makeBlankDevice = Impl.makeBlankDevice
  , addDevicePart = Impl.addDevicePart
  , getDevicePart = Impl.getDevicePart
  }
