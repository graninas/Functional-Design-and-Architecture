module Andromeda.Hardware.Impl.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import qualified Data.Map as Map



interpretHdlMethod
  :: RImpl.Runtime
  -> (RImpl.Runtime -> next -> IO RImpl.Runtime)
  -> L.HdlMethod next
  -> IO RImpl.Runtime

interpretHdlMethod runtime nextInterp (L.SetupController deviceName ctrlName passp next) = do
  let devices = RImpl._devices runtime
  let service = RImpl._hardwareService runtime

  eCtrlImpl <- SImpl.makeController service ctrlName passp
  case eCtrlImpl of
    Left err -> error err     -- bad practice
    Right ctrlImpl -> do
      blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
      let ctrl = T.Controller ctrlName
      let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
      let runtime' = RImpl.Runtime devices' service
      runtime'' <- nextInterp runtime' $ next ctrl
      pure runtime'

interpretHdlMethod runtime nextInterp (L.RegisterComponent ctrl idx passp) = do
  let devices = RImpl._devices runtime
  let service = RImpl._hardwareService runtime

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
          let runtime' = RImpl.Runtime devices' service
          pure runtime'






runHdl
  :: RImpl.Runtime
  -> (RImpl.Runtime -> next -> IO RImpl.Runtime)
  -> L.Hdl next
  -> IO RImpl.Runtime
runHdl runtime _ [] = pure runtime
runHdl runtime nextInterp (m:ms) = do
  devices' <- interpretHdlMethod runtime nextInterp m
  runHdl runtime nextInterp ms
