module Andromeda.Simulator.Control where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Simulator.Runtime as SimImpl
import qualified Andromeda.Simulator.Hardware.Interpreters.Hdl as SimImpl
import qualified Andromeda.Simulator.Hardware.Interpreters.DeviceControl as SimImpl
import qualified Andromeda.Simulator.LogicControl.Interpreter as SimImpl

import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Concurrent (ThreadId)


data SimulatorRequest
  = RunSimulation (L.LogicControl a) (MVar a)
  | ShutdownSimulator


data SimulatorControl = SimulatorControl
  { simulatorThreadId   :: ThreadId
  , simulatorRequestVar :: MVar SimulatorRequest
  }


simulatorWorker :: SimImpl.SimulatorRuntime -> MVar SimulatorRequest -> IO SimulatorControl
simulatorWorker runtime requestVar = do
  mbRequest <- tryReadMVar requestVar
  case mbRequest of
    Just (RunSimulation lc resultVar) -> do
      result <- SimImpl.runLogicControl runtime lc
      putMVar resultVar result
      threadDelay 1000
      simulatorWorker runtime requestVar
    Just ShutdownSimulator -> pure ()               -- TODO
    Nothing -> do
      threadDelay 1000
      simulatorWorker runtime requestVar

startSimulator :: SimImpl.SimulatorRuntime -> IO SimulatorControl
startSimulator runtime = do
  simRequestVar <- newEmptyMVar
  simThreadId <- forkIO $ simulatorWorker runtime simRequestVar
  pure $ SimulatorControl simThreadId simRequestVar

reportError :: SimulatorRuntime -> String -> IO ()
reportError SimulatorRuntime{_errorsVar} err = do
  errs <- takeMVar _errorsVar
  let errs' = err : errs
  putMVar _errorsVar errs'
