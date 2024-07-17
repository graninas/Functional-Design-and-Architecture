module Andromeda.Hardware.Impl.Interpreters.Hdl where


import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)


registerDevicePart
  :: RImpl.HardwareRuntime
  -> T.Controller
  -> T.ComponentIndex
  -> T.ComponentPassport
  -> IO ()
registerDevicePart RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} controller idx passp = do
  devices <- readIORef _devicesRef
  service <- readIORef _hardwareServiceRef

  case Map.lookup controller devices of
    Nothing -> error $ "Controller not found: " <> show controller     -- bad practice
    Just (_, device) -> do
      eDeivcePart <- SImpl.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> SImpl.addDevicePart service idx part device


registerDevice
  :: RImpl.HardwareRuntime
  -> (TImpl.ControllerImpl, TImpl.Device)
  -> T.Controller
  -> IO ()
registerDevice RImpl.HardwareRuntime{_devicesRef} deviceImpl ctrl = do
  devices <- readIORef _devicesRef
  writeIORef _devicesRef $ Map.insert ctrl deviceImpl devices


createController
  :: RImpl.HardwareRuntime
  -> T.ControllerName
  -> T.ComponentPassport
  -> IO TImpl.ControllerImpl
createController RImpl.HardwareRuntime{_hardwareServiceRef} ctrlName passp = do
  service <- readIORef _hardwareServiceRef

  eControllerImpl <- SImpl.makeController service ctrlName passp
  case eControllerImpl of
    Left err -> error err   -- bad practice
    Right controllerImpl -> pure controllerImpl


interpretHdlMethod :: RImpl.HardwareRuntime -> L.HdlMethod a -> IO a
interpretHdlMethod runtime (L.SetupController deviceName ctrlName passp next) = do
  let RImpl.HardwareRuntime {_devicesRef, _hardwareServiceRef} = runtime
  devices <- readIORef _devicesRef
  service <- readIORef _hardwareServiceRef

  eCtrlImpl <- SImpl.makeController service ctrlName passp
  case eCtrlImpl of
    Left err -> error err     -- bad practice
    Right ctrlImpl -> do
      blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
      let ctrl = T.Controller ctrlName
      let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
      writeIORef _devicesRef devices'
      pure $ next ctrl

interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
  let RImpl.HardwareRuntime {_devicesRef, _hardwareServiceRef} = runtime
  devices <- readIORef _devicesRef
  service <- readIORef _hardwareServiceRef

  let mbDevice = Map.lookup ctrl devices
  case mbDevice of
    Nothing -> error "Device not found"    -- bad practice
    Just (ctrlImpl, device) -> do
      eDeivcePart <- SImpl.makeDevicePart service passp
      case eDeivcePart of
        Left err -> error err    -- TODO: bad practice
        Right part -> do
          SImpl.addDevicePart service idx part device
          let devices' = Map.insert ctrl (ctrlImpl, device) devices
          writeIORef _devicesRef devices'
          pure $ next ()



runHdl :: RImpl.HardwareRuntime -> L.Hdl a -> IO a
runHdl runtime hdl = foldFree (interpretHdlMethod runtime) hdl
