module Andromeda.LogicControl.Impl.Interpreters.LogicControl where

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

import qualified Andromeda.Hardware.Impl.HdlInterpreter as HdlImpl

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L


import qualified Data.Map as Map







interpretLogicControlMethod
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> L.LogicControlMethod
  -> IO RImpl.Devices

interpretLogicControlMethod devices hardwareService (L.EvalHdl hdl) = do
  HdlImpl.runHdl devices hardwareService hdl


runLogicControl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> L.LogicControl
  -> IO RImpl.Devices
runLogicControl devices _ [] = pure devices
runLogicControl devices hardwareService (m:ms) = do
  devices' <- interpretLogicControlMethod devices hardwareService m
  runLogicControl devices' hardwareService ms
