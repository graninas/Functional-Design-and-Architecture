module Andromeda.Assets.Vendors.BBB.HardwareService where

import Andromeda.Hardware (HardwareService(..))
import qualified Andromeda.Hardware.Impl.Device as D
import Andromeda.Assets.Vendors.BBB.ComponentsAPI (bbbVendorComponents)

bbbHardwareService :: HardwareService
bbbHardwareService = HardwareService
  { makeDevice     = \hdl -> D.makeDevice bbbVendorComponents hdl
  , getBlankDevice = pure D.blankDevice
  , getDevicePart  = \idx device -> pure (D.getDevicePart idx device)
  }
