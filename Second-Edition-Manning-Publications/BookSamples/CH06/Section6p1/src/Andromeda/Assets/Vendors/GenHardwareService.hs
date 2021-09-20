module Andromeda.Assets.Vendors.GenHardwareService where

import Andromeda.Hardware
import qualified Andromeda.Hardware.Impl.Device as Impl
import Andromeda.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)
import Andromeda.Assets.Vendors.BBB.ComponentsAPI (bbbVendorComponents)

import qualified Data.Map as Map

allComponents :: VendorComponents
allComponents = Map.union
  aaaVendorComponents
  bbbVendorComponents

genHardwareService :: HardwareService
genHardwareService = HardwareService
  { makeController = Impl.makeController allComponents
  , makeDevicePart = Impl.makeDevicePart allComponents
  , makeBlankDevice = Impl.makeBlankDevice
  , addDevicePart = Impl.addDevicePart
  , getDevicePart = Impl.getDevicePart
  }
