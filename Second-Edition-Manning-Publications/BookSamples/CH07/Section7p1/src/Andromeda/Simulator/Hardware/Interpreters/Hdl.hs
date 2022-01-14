module Andromeda.Simulator.Hardware.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Simulator.Hardware.Device as Impl

import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)


type ControllerSimChannel = (MVar ControllerSimRequest, MVar ControllerSimResponse)
type DevicePartSimChannel = (MVar DevicePartSimRequest, MVar DevicePartSimResponse)


data DevicePartSim = DevicePartSim
  { devicePartSimThreadId :: ThreadId
  , devicePartSimDef :: ComponentPassport
  }


type DevicePartSims = Map ComponentIndex DevicePartSim

data ControllerSim = ControllerSim
  { ctrlSimThreadId :: ThreadId
  , ctrlSimDef :: (ControllerName, ComponentPassport)
  , ctrlSimDevicePartsVar :: MVar DevicePartSims
  , ctrlSimChannel :: ControllerSimChannel
  }




createControllerSimChannel :: IO ControllerSimChannel
createControllerSimChannel = do
  requestMVar <- newEmptyMVar
  responseMVar <- newEmptyMVar
  pure (requestMVar, responseMVar)


createDevicePartSimChannel :: IO DevicePartSimChannel
createDevicePartSimChannel = do
  requestMVar <- newEmptyMVar
  responseMVar <- newEmptyMVar
  pure (requestMVar, responseMVar)



makeControllerSim
  :: ControllerName
  -> ComponentPassport
  -> IO (Either String ControllerSim)
makeControllerSim ctrlName ctrlPassp@(ComponentPassport Controllers _ _ _) = do
  devicePartsVar <- newMVar Map.empty
  channel <- createControllerSimChannel devicePartsVar
  threadId <- forkIO (controllerWorker channel)
  let sim = ControllerSim
        { ctrlSimThreadId = threadId
        , ctrlSimDef = (ctrlName, ctrlPassp)
        , ctrlSimDevicePartsVar = devicePartsVar
        , ctrlSimChannel = channel
        }
  pure $ Right sim
makeControllerSim _ _ = pure $ Left "Invalid/unknown component class for a controller"



makeDevicePartSim
  :: ComponentPassport
  -> IO (Either String DevicePartSim)
makeDevicePartSim passp@(ComponentPassport (Sensors param) _ _ _) = do
  channel <- createDevicePartSimChannel
  threadId <- forkIO (sensorWorker channel param)
  let sim = DevicePartSim
        { devicePartSimThreadId = threadId
        , devicePartSimDef = passp
        , devicePartSimChannel = channel
        }
  pure $ Right sim
makeDevicePartSim _ _ = pure $ Left "Invalid/unknown component class for a device part"



interpretHdlMethod :: RImpl.SimulatorRuntime -> L.HdlMethod a -> IO a

interpretHdlMethod runtime (L.SetupController deviceName ctrlName passp next) = do
  eCtrlSim <- SImpl.makeControllerSim service ctrlName passp
  case eCtrlSim of
    Left err -> error err     -- bad practice
    Right ctrlSim -> do
      let RImpl.SimulatorRuntime {_controllerSimsRef} = runtime
      controllerSims <- readIORef _controllerSimsRef
      let ctrl = T.Controller ctrlName
      let controllerSims' = Map.insert ctrl ctrlSim
      writeIORef _controllerSimsRef controllerSims'
      pure $ next ctrl


interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
  let RImpl.SimulatorRuntime {_controllerSimsRef} = runtime
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



runHdl :: RImpl.SimulatorRuntime -> L.Hdl a -> IO a
runHdl runtime hdl = foldFree (interpretHdlMethod runtime) hdl
