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
  :: RImpl.Runtime
  -> L.LogicControlMethod
  -> IO RImpl.Runtime

interpretLogicControlMethod runtime (L.EvalHdl hdl) =
  HdlImpl.runHdl runtime runLogicControl hdl

interpretLogicControlMethod runtime (L.EvalDeviceControl dc) =
  DeviceControlImpl.runDeviceControl runtime runLogicControl dc

interpretLogicControlMethod runtime (L.Report msg) = do
  putStrLn msg
  pure runtime

interpretLogicControlMethod runtime (L.Store k v) =
  error "not implemented"



runLogicControl
  :: RImpl.Runtime
  -> L.LogicControl
  -> IO RImpl.Runtime
runLogicControl runtime [] = pure runtime
runLogicControl runtime (m:ms) = do
  runtime' <- interpretLogicControlMethod runtime m
  runLogicControl runtime' ms
