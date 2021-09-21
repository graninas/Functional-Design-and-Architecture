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
import Control.Monad.Free (foldFree)

registerDevicePart
  :: RImpl.HardwareRuntime
  -> SImpl.HardwareService
  -> T.Controller
  -> T.ComponentIndex
  -> T.ComponentPassport
  -> IO ()
registerDevicePart RImpl.HardwareRuntime{devicesRef} service controller idx passp = do
  devices <- readIORef devicesRef
  case Map.lookup controller devices of
    Nothing -> error $ "Controller not found: " <> show controller     -- bad practice
    Just device -> do
      eDeivcePart <- SImpl.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> SImpl.addDevicePart service idx part device


createController
  :: SImpl.HardwareService
  -> T.ControllerName
  -> T.ComponentPassport
  -> IO TImpl.ControllerImpl
createController service ctrlName passp = do
  eControllerImpl <- SImpl.makeController service ctrlName passp
  case eControllerImpl of
    Left err -> error err   -- bad practice
    Right controllerImpl -> pure controllerImpl

registerDevice
  :: RImpl.HardwareRuntime
  -> TImpl.Device
  -> T.Controller
  -> IO ()
registerDevice RImpl.HardwareRuntime{devicesRef} device ctrl = do
  devices <- readIORef devicesRef
  writeIORef devicesRef $ Map.insert ctrl device devices



interpretHdlMethod :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.HdlMethod a -> IO a
interpretHdlMethod runtime service (L.SetupController deviceName ctrlName passp next) = do
  ctrlImpl <- createController service ctrlName passp
  blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
  let ctrl = T.Controller ctrlName
  registerDevice runtime blankDevice ctrl
  pure $ next ctrl

interpretHdlMethod runtime service (L.RegisterComponent controller idx passp next) = do
  registerDevicePart runtime service controller idx passp
  pure $ next ()

runHdl :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.Hdl a -> IO a
runHdl runtime service hdl = foldFree (interpretHdlMethod runtime service) hdl
