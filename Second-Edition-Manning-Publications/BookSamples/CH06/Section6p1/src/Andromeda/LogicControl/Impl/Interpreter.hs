module Andromeda.LogicControl.Impl.Interpreter where

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Hardware.Impl.Runtime as Impl
import qualified Andromeda.Hardware.Impl.Interpreter as Impl
import qualified Andromeda.Hardware.Impl.Service as Impl


import Control.Monad.Free (foldFree)


interpretLogicControlMethod
  :: Impl.HardwareRuntime
  -> Impl.HardwareService
  -> L.LogicControlMethod a
  -> IO a
interpretLogicControlMethod hardwareRuntime hardwareService
  (L.EvalHdl hdl next) = do
    res <- Impl.runHdl hardwareRuntime hardwareService hdl
    pure $ next res




runLogicControl
  :: Impl.HardwareRuntime
  -> Impl.HardwareService
  -> L.LogicControl a
  -> IO a
runLogicControl hardwareRuntime hardwareService lControl =
  foldFree (interpretLogicControlMethod hardwareRuntime hardwareService) lControl
