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










runLogicControl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (nextIterp -> IO ())
  -> L.Hdl nextIterp
  -> IO RImpl.Devices
runLogicControl _ _ _ _ = error "not implemented"
