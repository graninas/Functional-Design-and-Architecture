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


import qualified Data.Map as Map






interpretHdlMethod :: RImpl.Devices -> SImpl.HardwareService -> L.HdlMethod -> IO RImpl.Devices
interpretHdlMethod devices service (L.SetupController deviceName ctrlName passp next) = do
  eCtrlImpl <- SImpl.makeController service ctrlName passp
  case eCtrlImpl of
    Left err -> error err     -- bad practice
    Right ctrlImpl -> do
      blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
      let ctrl = T.Controller ctrlName
      let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
      devices'' <- runHdl devices' service $ next ctrl
      pure devices''

interpretHdlMethod devices service (L.RegisterComponent ctrl idx passp) = do
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

interpretHdlMethod _ _ _ = error "Not implemented"


runHdl :: RImpl.Devices -> SImpl.HardwareService -> L.Hdl -> IO RImpl.Devices
runHdl devices _ [] = pure devices
runHdl devices service (hdlMethod:ms) = do
  devices' <- interpretHdlMethod devices service hdlMethod
  runHdl devices' service ms
