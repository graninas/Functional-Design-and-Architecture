module Andromeda.Simulator.Runtime where

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device

import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as Map




data SimulatorRuntime = SimulatorRuntime
  { _controllerSimsVar :: MVar (Map.Map T.Controller ControllerSim)
  , _messagesVar       :: MVar [String]
  , _errorsVar         :: MVar [String]
  }



createSimulatorRuntime :: IO SimulatorRuntime
createSimulatorRuntime = do
  simsVar <- newMVar Map.empty
  msgsVar <- newMVar []
  errsVar <- newMVar []
  pure $ SimulatorRuntime simsVar msgsVar errsVar


reportError :: SimulatorRuntime -> String -> IO ()
reportError SimulatorRuntime{_errorsVar} err = do
  errs <- takeMVar _errorsVar
  let errs' = err : errs
  putMVar _errorsVar errs'
