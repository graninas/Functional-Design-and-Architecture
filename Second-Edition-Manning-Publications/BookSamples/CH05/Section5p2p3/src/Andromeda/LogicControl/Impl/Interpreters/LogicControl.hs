module Andromeda.LogicControl.Impl.Interpreters.LogicControl where

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Impl.Interpreters.DeviceControl as DeviceControlImpl

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L


import qualified Data.Map as Map







interpretLogicControlMethod
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> L.LogicControlMethod
  -> IO RImpl.Devices

interpretLogicControlMethod devices hardwareService (L.EvalHdl hdl) = do
  let interp ds lc = runLogicControl ds hardwareService lc
  HdlImpl.runHdl devices hardwareService interp hdl

interpretLogicControlMethod devices hardwareService (L.EvalDeviceControl dc) = do
  let interp ds lc = runLogicControl ds hardwareService lc
  DeviceControlImpl.runDeviceControl devices hardwareService interp dc

interpretLogicControlMethod devices hardwareService (L.Report msg) = do
  putStrLn msg
  pure devices

interpretLogicControlMethod devices hardwareService (L.Store k v) = do
  error "not implemented"



runLogicControl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> L.LogicControl
  -> IO RImpl.Devices
runLogicControl devices _ [] = pure devices
runLogicControl devices hardwareService (m:ms) = do
  devices' <- interpretLogicControlMethod devices hardwareService m
  runLogicControl devices hardwareService ms
