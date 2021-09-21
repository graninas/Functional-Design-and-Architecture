module Andromeda.Hardware.Impl.HdlInterpreter where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import Data.IORef
import qualified Data.Map as Map




type Devices = Map.Map ControllerName (ControllerImpl, Device)





interpretHdlMethod :: Devices -> SImpl.HardwareService -> L.HdlMethod -> IO Devices
interpretHdlMethod devices service (L.SetupController deviceName ctrlName passp) = do
  ctrlImpl <- createController service ctrlName passp
  blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
  let devices' = Map.insert ctrlName (ctrlImpl, blankDevice)
  pure devices'

interpretHdlMethod devices service (L.RegisterComponent controller idx passp) = do
  let mbDevice = Map.lookup (controller, idx) devices
  case mbDevice of
    Nothing -> error "Device not found"    -- bad practice
    Just (_, device) -> do
      eDeivcePart <- SImpl.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> do
          device' <- SImpl.addDevicePart service idx part device
          let devices' = Map.insert (controller, idx) device'
          pure devices'



runHdl :: Devices -> SImpl.HardwareService -> L.Hdl -> IO Devices
runHdl devices _ [] = devices
runHdl devices service (hdlMethod:ms) = do
  devices' <- interpretHdlMethod devices service hdlMethod
  runHdl devices service devices'
