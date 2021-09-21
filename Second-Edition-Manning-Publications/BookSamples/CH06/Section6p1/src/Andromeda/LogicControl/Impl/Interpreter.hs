module Andromeda.LogicControl.Impl.Interpreter where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.HdlInterpreter as HdlImpl
import qualified Andromeda.Hardware.Impl.HilInterpreter as HilImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl


import Control.Monad.Free (foldFree)


interpretLogicControlMethod
  :: RImpl.HardwareRuntime
  -> SImpl.HardwareService
  -> L.LogicControlMethod a
  -> IO a
interpretLogicControlMethod hardwareRuntime hardwareService
  (L.EvalHdl hdl next) = do
    res <- HdlImpl.runHdl hardwareRuntime hardwareService hdl
    pure $ next res

interpretLogicControlMethod hardwareRuntime hardwareService
  (L.EvalHil hil next) = do
    res <- HilImpl.runHil hardwareRuntime hardwareService hil
    pure $ next res

interpretLogicControlMethod hardwareRuntime hardwareService
  (L.Report msg next) = do
    pure $ next ()

interpretLogicControlMethod hardwareRuntime hardwareService
  (L.Store key value next) = do
    pure $ next ()




runLogicControl
  :: RImpl.HardwareRuntime
  -> SImpl.HardwareService
  -> L.LogicControl a
  -> IO a
runLogicControl hardwareRuntime hardwareService lControl =
  foldFree (interpretLogicControlMethod hardwareRuntime hardwareService) lControl
