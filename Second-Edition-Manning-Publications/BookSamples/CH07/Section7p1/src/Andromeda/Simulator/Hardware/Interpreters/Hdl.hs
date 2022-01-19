module Andromeda.Simulator.Hardware.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device
import Andromeda.Simulator.Runtime

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad.Free (foldFree)
import Control.Monad (forever)



controllerWorker :: MVar ControllerSimRequest -> IO ()
controllerWorker requestVar = forever (pure ())

sensorWorker :: MVar DevicePartSimRequest -> T.Parameter -> IO ()
sensorWorker requestVar _ = forever (pure ())



makeControllerSim
  :: T.ControllerName
  -> T.ComponentPassport
  -> IO (Either String ControllerSim)
makeControllerSim ctrlName ctrlPassp@(T.ComponentPassport T.Controllers _ _ _) = do
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
  :: T.ComponentPassport
  -> IO (Either String DevicePartSim)
makeDevicePartSim passp@(T.ComponentPassport (T.Sensors param) _ _ _) = do
  requestVar <- newEmptyMVar
  threadId <- forkIO (sensorWorker requestVar param)
  let sim = DevicePartSim
        { devicePartSimThreadId = threadId
        , devicePartSimDef = passp
        , devicePartSimRequestVar = requestVar
        }
  pure $ Right sim
makeDevicePartSim _ = pure $ Left "Invalid/unknown component class for a device part"



interpretHdlMethod :: SimulatorRuntime -> L.HdlMethod a -> IO a

interpretHdlMethod runtime (L.SetupController deviceName ctrlName passp next) = do
  eCtrlSim <- makeControllerSim ctrlName passp
  case eCtrlSim of
    Left err -> do
      reportError runtime err
      error err                        -- bad practice
    Right ctrlSim -> do
      let SimulatorRuntime {_controllerSimsVar} = runtime
      controllerSims <- takeMVar _controllerSimsVar
      let ctrl = T.Controller ctrlName
      let controllerSims' = Map.insert ctrl ctrlSim controllerSims
      putMVar _controllerSimsVar controllerSims'
      pure $ next ctrl


interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
  let SimulatorRuntime {_controllerSimsVar} = runtime
  controllerSims <- takeMVar _controllerSimsVar

  let mbCtrlSim = Map.lookup ctrl controllerSims
  let tryRegisterComponent = case mbCtrlSim of
        Nothing -> do
          reportError runtime "Controller sim not found"
          error "Controller sim not found"     -- bad practice
        Just (ControllerSim _ _ partsVar _) -> do
          eDeivcePartSim <- makeDevicePartSim passp
          case eDeivcePartSim of
            Left err -> do
              reportError runtime err
              error err                        -- bad practice
            Right devicePartSim -> do
              parts <- takeMVar partsVar
              let parts' = Map.insert idx devicePartSim parts
              putMVar partsVar parts'

  tryRegisterComponent
  putMVar _controllerSimsVar controllerSims
  pure $ next ()



runHdl :: SimulatorRuntime -> L.Hdl a -> IO a
runHdl runtime hdl = foldFree (interpretHdlMethod runtime) hdl
