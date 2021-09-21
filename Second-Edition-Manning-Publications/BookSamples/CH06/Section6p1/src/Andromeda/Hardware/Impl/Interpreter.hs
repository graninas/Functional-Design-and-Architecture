module Andromeda.Hardware.Impl.Interpreter where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as S
import qualified Andromeda.Hardware.Impl.Device.Types as Impl
import qualified Andromeda.Hardware.Impl.Runtime as Impl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)

registerDevicePart
  :: Impl.HardwareRuntime
  -> S.HardwareService
  -> T.Controller
  -> T.ComponentIndex
  -> T.ComponentPassport
  -> IO ()
registerDevicePart Impl.HardwareRuntime{devicesRef} service controller idx passp = do
  devices <- readIORef devicesRef
  case Map.lookup controller devices of
    Nothing -> error $ "Controller not found: " <> show controller     -- bad practice
    Just device -> do
      eDeivcePart <- S.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> S.addDevicePart service idx part device


createController
  :: S.HardwareService
  -> T.ControllerName
  -> T.ComponentPassport
  -> IO Impl.ControllerImpl
createController service ctrlName passp = do
  eControllerImpl <- S.makeController service ctrlName passp
  case eControllerImpl of
    Left err -> error err   -- bad practice
    Right controllerImpl -> pure controllerImpl

registerDevice
  :: Impl.HardwareRuntime
  -> Impl.Device
  -> T.Controller
  -> IO ()
registerDevice Impl.HardwareRuntime{devicesRef} device ctrl = do
  devices <- readIORef devicesRef
  writeIORef devicesRef $ Map.insert ctrl device devices



interpretHdlMethod :: Impl.HardwareRuntime -> S.HardwareService -> L.HdlMethod a -> IO a
interpretHdlMethod runtime service (L.RegisterComponent controller idx passp next) = do
  registerDevicePart runtime service controller idx passp
  pure $ next ()

interpretHdlMethod runtime service (L.SetupController deviceName ctrlName passp next) = do
  ctrlImpl <- createController service ctrlName passp
  blankDevice <- S.makeBlankDevice service deviceName ctrlImpl
  let ctrl = T.Controller ctrlName
  registerDevice runtime blankDevice ctrl
  pure $ next ctrl

interpretHdlMethod runtime service (L.GetStatus ctrl next) =
  -- TODO: dummy
  pure $ next T.StatusOk


runHdl :: Impl.HardwareRuntime -> S.HardwareService -> L.Hdl a -> IO a
runHdl runtime service hdl = foldFree (interpretHdlMethod runtime service) hdl
