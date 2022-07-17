module Andromeda.Simulator.LogicControl.Interpreter where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import Andromeda.Simulator.Runtime
import Andromeda.Simulator.Hardware.Interpreters.Hdl
import Andromeda.Simulator.Hardware.Interpreters.DeviceControl

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId)
import Control.Monad.Free (foldFree)


interpretLogicControlMethod
  :: SimulatorRuntime
  -> L.LogicControlMethod a
  -> IO a
interpretLogicControlMethod runtime (L.EvalHdl hdl next) = do
  res <- runHdl runtime hdl
  pure $ next res

interpretLogicControlMethod runtime (L.EvalDeviceControlMethod dc next) = do
  res <- interpretDeviceControlMethod runtime dc
  pure $ next res

interpretLogicControlMethod runtime (L.Report msg next) = do
  let SimulatorRuntime{simRtMessagesVar} = runtime
  msgs <- takeMVar simRtMessagesVar
  let msgs' = msg : msgs
  putMVar simRtMessagesVar msgs'
  pure $ next ()

interpretLogicControlMethod runtime (L.Store key value next) = do
  let SimulatorRuntime{simKeyValueDBVar} = runtime
  kvDB <- takeMVar simKeyValueDBVar
  putMVar simKeyValueDBVar $ Map.insert key value kvDB
  pure $ next ()

interpretLogicControlMethod runtime (L.Load key next) = do
  let SimulatorRuntime{simKeyValueDBVar} = runtime
  kvDB <- readMVar simKeyValueDBVar
  pure $ next $ Map.lookup key kvDB




runLogicControl
  :: SimulatorRuntime
  -> L.LogicControl a
  -> IO a
runLogicControl runtime (L.LogicControl lControl) =
  foldFree (interpretLogicControlMethod runtime) lControl
