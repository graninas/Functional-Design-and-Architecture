module Andromeda.Simulator.Runtime where

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import Andromeda.Simulator.Hardware.Device

import Data.IORef
import Control.Concurrent.MVar
import qualified Data.Map as Map




data SimulatorRuntime = SimulatorRuntime
  { _controllerSimsRef :: IORef (Map.Map T.Controller ControllerSim)
  }



createSimulatorRuntime :: IO SimulatorRuntime
createSimulatorRuntime = do
  sims <- newIORef
  pure $ SimulatorRuntime sims
