module Andromeda.Hardware.Impl.Interpreters.Hdl where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import qualified Data.Map as Map










runHdl
  :: RImpl.Devices
  -> SImpl.HardwareService
  -> (next -> IO ())
  -> L.Hdl next
  -> IO RImpl.Devices
runHdl _ _ _ _ = error "not implemented"
