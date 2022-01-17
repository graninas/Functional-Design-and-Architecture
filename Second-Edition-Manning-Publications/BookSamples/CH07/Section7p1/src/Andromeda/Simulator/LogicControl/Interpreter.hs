module Andromeda.Simulator.LogicControl.Interpreter where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import Andromeda.Simulator.Hardware.Interpreters.Hdl
import Andromeda.Simulator.Hardware.Interpreters.DeviceControl

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
  let SimulatorRuntime{_messagesVar} = runtime
  msgs <- takeMVar _messagesVar
  let msgs' = msg : msgs
  putMVar _messagesVar msgs'
  pure $ next ()

interpretLogicControlMethod runtime
  (L.Store key value next) = do
    pure $ next ()




runLogicControl
  :: SimulatorRuntime
  -> L.LogicControl a
  -> IO a
runLogicControl runtime lControl =
  foldFree (interpretLogicControlMethod runtime) lControl
