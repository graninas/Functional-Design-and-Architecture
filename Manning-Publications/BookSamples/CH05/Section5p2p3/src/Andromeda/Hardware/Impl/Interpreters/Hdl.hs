module Andromeda.Hardware.Impl.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl


import qualified Data.Map as Map






interpretHdlMethod
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (RImpl.Devices -> next -> IO RImpl.Devices)
  -> L.HdlMethod next
  -> IO RImpl.Devices

interpretHdlMethod devices service nextInterp
  (L.SetupController deviceName ctrlName passp next) = do
  eCtrlImpl <- SImpl.makeController service ctrlName passp
  case eCtrlImpl of
    Left err -> error err     -- bad practice
    Right ctrlImpl -> do
      blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
      let ctrl = T.Controller ctrlName
      let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
      let nextScript = next ctrl
      nextInterp devices' nextScript

interpretHdlMethod devices service nextInterp
  (L.RegisterComponent ctrl idx passp) = do
  let mbDevice = Map.lookup ctrl devices
  case mbDevice of
    Nothing -> error "Device not found"    -- bad practice
    Just (ctrlImpl, device) -> do
      eDeivcePart <- SImpl.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> do
          device' <- SImpl.addDevicePart service idx part device
          let devices' = Map.insert ctrl (ctrlImpl, device') devices
          pure devices'


runHdl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (RImpl.Devices -> next -> IO RImpl.Devices)
  -> L.Hdl next
  -> IO RImpl.Devices
runHdl devices _ _ [] = pure devices
runHdl devices service nextInterp (m:ms) = do
  devices' <- interpretHdlMethod devices service nextInterp m
  runHdl devices' service nextInterp ms
