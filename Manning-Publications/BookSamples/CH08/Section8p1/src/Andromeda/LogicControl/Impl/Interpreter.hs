module Andromeda.LogicControl.Impl.Interpreter where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Impl.Interpreters.DeviceControl as DCImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl


import Control.Monad.Free (foldFree)


interpretLogicControlMethod
  :: RImpl.HardwareRuntime
  -> L.LogicControlMethod a
  -> IO a
interpretLogicControlMethod hardwareRuntime
  (L.EvalHdl hdl next) = do
    res <- HdlImpl.runHdl hardwareRuntime hdl
    pure $ next res

interpretLogicControlMethod hardwareRuntime
  (L.EvalDeviceControlMethod dc next) = do
    res <- DCImpl.interpretDeviceControlMethod hardwareRuntime dc
    pure $ next res

interpretLogicControlMethod hardwareRuntime
  (L.Report msg next) = do
    pure $ next ()

interpretLogicControlMethod hardwareRuntime
  (L.Store key value next) = do
    pure $ next ()

interpretLogicControlMethod hardwareRuntime
  (L.Load key next) = error "Load not implemented"


runLogicControl
  :: RImpl.HardwareRuntime
  -> L.LogicControl a
  -> IO a
runLogicControl hardwareRuntime (L.LogicControl lControl) =
  foldFree (interpretLogicControlMethod hardwareRuntime) lControl
