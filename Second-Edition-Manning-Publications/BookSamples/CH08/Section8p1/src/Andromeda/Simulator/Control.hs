{-# LANGUAGE GADTs #-}

module Andromeda.Simulator.Control where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Simulator.Runtime as SimImpl
import qualified Andromeda.Simulator.Hardware.Device as SimImpl
import qualified Andromeda.Simulator.Hardware.Interpreters.Hdl as SimImpl
import qualified Andromeda.Simulator.Hardware.Interpreters.DeviceControl as SimImpl
import qualified Andromeda.Simulator.LogicControl.Interpreter as SimImpl

import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)


newtype RunResult a    = RunResult (MVar a)
newtype ShutdownResult = ShutdownResult (MVar ())

data SimulatorRequest
  = forall a. RunSimulation (L.LogicControl a) (RunResult a)
  | StartSimulation (L.LogicControl ())
  | ShutdownSimulator ShutdownResult


data SimulatorControl = SimulatorControl
  { simulatorThreadId   :: ThreadId
  , simulatorRequestVar :: MVar SimulatorRequest
  }


simulatorWorker :: SimImpl.SimulatorRuntime -> MVar SimulatorRequest -> IO ()
simulatorWorker runtime requestVar = do
  mbRequest <- tryReadMVar requestVar
  case mbRequest of
    Just (RunSimulation lc (RunResult resultVar)) -> do
      result <- SimImpl.runLogicControl runtime lc
      putMVar resultVar result
      threadDelay 1000
      simulatorWorker runtime requestVar
    Just (StartSimulation lc) -> do
      simThreadId <- forkIO $ SimImpl.runLogicControl runtime lc
      sims <- takeMVar $ SimImpl.simSimulationsVar runtime
      putMVar (SimImpl.simSimulationsVar runtime) (simThreadId : sims)
      threadDelay 1000
      simulatorWorker runtime requestVar
    Just (ShutdownSimulator (ShutdownResult resultVar)) -> do
      stopSimulation runtime
      putMVar resultVar ()
    Nothing -> do
      threadDelay 1000
      simulatorWorker runtime requestVar

startSimulator :: SimImpl.SimulatorRuntime -> IO SimulatorControl
startSimulator runtime = do
  simRequestVar <- newEmptyMVar
  simThreadId <- forkIO $ simulatorWorker runtime simRequestVar
  pure $ SimulatorControl simThreadId simRequestVar

runSimulation :: SimulatorControl -> L.LogicControl a -> IO a
runSimulation control lc = do
  let SimulatorControl {simulatorRequestVar} = control
  resultVar <- newEmptyMVar
  putMVar simulatorRequestVar $ RunSimulation lc (RunResult resultVar)
  takeMVar resultVar

-- TODO: write this function
stopSimulation :: SimImpl.SimulatorRuntime -> IO ()
stopSimulation _ = pure ()



stopSimulator :: SimImpl.SimulatorRuntime -> SimulatorControl -> IO ()
stopSimulator runtime SimulatorControl {simulatorThreadId} = do
  killThread simulatorThreadId
  stopAllSims runtime

stopAllSims :: SimImpl.SimulatorRuntime -> IO ()
stopAllSims SimImpl.SimulatorRuntime {simRtControllerSimsVar} = do
  sims <- readMVar simRtControllerSimsVar
  mapM_ stopSim $ Map.elems sims
  where
    stopDevicePartSim SimImpl.DevicePartSim {devicePartSimThreadId} =
      killThread devicePartSimThreadId
    stopSim SimImpl.ControllerSim {ctrlSimThreadId, ctrlSimDevicePartsVar} = do
      killThread ctrlSimThreadId
      parts <- readMVar ctrlSimDevicePartsVar
      mapM_ stopDevicePartSim $ Map.elems parts
