module Andromeda.Simulator.Hardware.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device
import Andromeda.Simulator.Runtime

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Monad.Free (foldFree)



controllerWorker :: IO ThreadId
controllerWorker = forever (pure ())

sensorWorker :: Parameter -> IO ThreadId
sensorWorker _ = forever (pure ())



makeControllerSim
  :: ControllerName
  -> ComponentPassport
  -> IO (Either String ControllerSim)
makeControllerSim ctrlName ctrlPassp@(ComponentPassport Controllers _ _ _) = do
  devicePartsVar <- newMVar Map.empty
  requestVar <- newEmptyMVar
  threadId <- forkIO (controllerWorker requestVar)
  let sim = ControllerSim
        { ctrlSimThreadId = threadId
        , ctrlSimDef = (ctrlName, ctrlPassp)
        , ctrlSimDevicePartsVar = devicePartsVar
        , ctrlSimRequestVar = requestVar
        }
  pure $ Right sim
makeControllerSim _ _ = pure $ Left "Invalid/unknown component class for a controller"



makeDevicePartSim
  :: ComponentPassport
  -> IO (Either String DevicePartSim)
makeDevicePartSim passp@(ComponentPassport (Sensors param) _ _ _) = do
  requestVar <- newEmptyMVar
  threadId <- forkIO (sensorWorker requestVar param)
  let sim = DevicePartSim
        { devicePartSimThreadId = threadId
        , devicePartSimDef = passp
        , devicePartSimRequestVar = requestVar
        }
  pure $ Right sim
makeDevicePartSim _ _ = pure $ Left "Invalid/unknown component class for a device part"



interpretHdlMethod :: SimulatorRuntime -> L.HdlMethod a -> IO a

interpretHdlMethod runtime (L.SetupController deviceName ctrlName passp next) = do
  eCtrlSim <- SImpl.makeControllerSim service ctrlName passp
  case eCtrlSim of
    Left err -> error err     -- bad practice
    Right ctrlSim -> do
      let SimulatorRuntime {_controllerSimsRef} = runtime
      controllerSims <- readIORef _controllerSimsRef
      let ctrl = T.Controller ctrlName
      let controllerSims' = Map.insert ctrl ctrlSim
      writeIORef _controllerSimsRef controllerSims'
      pure $ next ctrl


interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
  let SimulatorRuntime {_controllerSimsRef} = runtime
  controllerSims <- readIORef _controllerSimsRef

  let mbCtrlSim = Map.lookup ctrl controllerSims
  case mbCtrlSim of
    Nothing -> error "Controller sim not found"    -- bad practice
    Just (ControllerSim thId def partsVar) -> do
      eDeivcePartSim <- makeDevicePartSim service passp
      case eDeivcePartSim of
        Left err -> error err    -- TODO: bad practice
        Right devicePartSim -> do
          parts <- takeMVar partsVar
          let parts' = Map.insert idx devicePartSim parts
          putMVar partsVar parts'
          pure $ next ()



runHdl :: SimulatorRuntime -> L.Hdl a -> IO a
runHdl runtime hdl = foldFree (interpretHdlMethod runtime) hdl