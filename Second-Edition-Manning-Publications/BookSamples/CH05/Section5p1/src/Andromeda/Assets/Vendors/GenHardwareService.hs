module Andromeda.Assets.Vendors.GenHardwareService where

import Andromeda.Hardware
import qualified Andromeda.Hardware.Impl.Device as D
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)
import Andromeda.Assets.Vendors.BBB.ComponentsAPI (bbbVendorComponents)

import qualified Data.Map as Map

allComponents :: VendorComponents
allComponents = Map.union
  aaaVendorComponents
  bbbVendorComponents

genHardwareService :: HardwareService
genHardwareService = HardwareService
  { makeDevice     = \hdl -> D.makeDevice allComponents hdl
  , getBlankDevice = pure D.blankDevice
  , getDevicePart  = \idx device -> pure (D.getDevicePart idx device)
  }
