module Andromeda.Assets.Vendors.AAA.HardwareService where

import Andromeda.Hardware (HardwareService(..))
import qualified Andromeda.Hardware.Impl.Device as D
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)

aaaHardwareService :: HardwareService
aaaHardwareService = HardwareService
  { makeDevice     = \hdl -> D.makeDevice aaaVendorComponents hdl
  , getBlankDevice = pure D.blankDevice
  , getDevicePart  = \idx device -> pure (D.getDevicePart idx device)
  }
