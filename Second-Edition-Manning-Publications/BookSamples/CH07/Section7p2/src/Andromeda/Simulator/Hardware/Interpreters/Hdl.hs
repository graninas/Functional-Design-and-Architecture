module Andromeda.Simulator.Hardware.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Common.Value as T
import qualified Andromeda.Common.Property as T
import qualified Andromeda.Common.Physics as T
import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device
import Andromeda.Simulator.Runtime

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.Free (foldFree)
import System.Random (randomRIO)


readSensorSimMeasurement
  :: MVar DevicePartSims
  -> T.ComponentIndex
  -> MVar (Maybe T.SensorMeasurement)
  -> IO ()
readSensorSimMeasurement partSimsVar idx mbMeasurementVar = do
  partSims <- readMVar partSimsVar
  case Map.lookup idx partSims of
    Nothing -> putMVar mbMeasurementVar Nothing
    Just DevicePartSim{devicePartSimRequestVar} ->
      putMVar devicePartSimRequestVar $ ProduceMeasurement mbMeasurementVar


controllerWorker
  :: MVar DevicePartSims
  -> MVar ControllerSimRequest
  -> T.Frequency
  -> T.ControllerStatus
  -> IO ()
controllerWorker partSimsVar requestVar hertz@(T.Hertz frequency) status = do
  let cont = controllerWorker partSimsVar requestVar hertz status

  mbRequest <- tryTakeMVar requestVar
  let (act, next) = case mbRequest of
        Nothing -> (pure (), cont)
        Just (SetControlerSimStatus newStatus) ->
          ( pure ()
          , controllerWorker partSimsVar requestVar hertz newStatus
          )
        Just (GetControlerSimStatus statusVar) ->
          ( putMVar statusVar status
          , cont
          )
        Just (ReadSimSensor idx mbMeasurementVar) ->
          ( readSensorSimMeasurement partSimsVar idx mbMeasurementVar
          , cont
          )
  act
  -- TODO: correct formula
  threadDelay $ ceiling $ (1.0 / (fromIntegral frequency)) * 1000.0
  next



sensorWorker
  :: MVar DevicePartSimRequest
  -> T.SensorType
  -> T.Frequency
  -> (Int, Int)
  -> IO ()
sensorWorker requestVar sensorType hertz@(T.Hertz frequency) range = do
  let cont = sensorWorker requestVar sensorType hertz range

  mbRequest <- tryTakeMVar requestVar
  let (act, next) = case mbRequest of
        Nothing -> (pure (), cont)
        Just (SetSensorSimRange newRange) ->
          ( pure ()
          , sensorWorker requestVar sensorType hertz newRange
          )
        Just (ProduceMeasurement measurementVar) ->
          ( do
              rndVal <- randomRIO range
              let valF = case sensorType of
                   T.TemperatureSensor -> T.SensorMeasurement . T.UnitTemperature . T.Kelvin
                   T.PressureSensor    -> T.SensorMeasurement . T.UnitPressure . T.Pascal
              putMVar measurementVar $ Just $ valF $ fromIntegral rndVal
          , cont
          )
  act
  -- TODO: correct formula
  threadDelay $ ceiling $ (1.0 / (fromIntegral frequency)) * 1000.0
  next


makeControllerSim
  :: T.ControllerName
  -> T.ComponentPassport
  -> IO (Either String ControllerSim)
makeControllerSim ctrlName ctrlPassp@(T.ComponentPassport T.Controllers _ _ _) = do

  -- default values, should be configured via the simulation model
  let frequency = T.Hertz 1000

  devicePartsVar <- newMVar Map.empty
  requestVar <- newEmptyMVar
  threadId <- forkIO (controllerWorker devicePartsVar requestVar frequency T.ControllerOk)
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
makeDevicePartSim passp@(T.ComponentPassport (T.Sensors sensorType) _ _ _) = do

  -- default values, should be configured via the simulation model
  let frequency = T.Hertz 1000
  let measurementRange = (100, 200)

  requestVar <- newEmptyMVar
  threadId <- forkIO (sensorWorker requestVar sensorType frequency measurementRange)
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
      let SimulatorRuntime {simRtControllerSimsVar} = runtime
      controllerSims <- takeMVar simRtControllerSimsVar
      let ctrl = T.Controller ctrlName
      let controllerSims' = Map.insert ctrl ctrlSim controllerSims
      putMVar simRtControllerSimsVar controllerSims'
      pure $ next ctrl


interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
  let SimulatorRuntime {simRtControllerSimsVar} = runtime

  controllerSims <- takeMVar simRtControllerSimsVar

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
  putMVar simRtControllerSimsVar controllerSims
  pure $ next ()



runHdl :: SimulatorRuntime -> L.Hdl a -> IO a
runHdl runtime hdl = foldFree (interpretHdlMethod runtime) hdl
