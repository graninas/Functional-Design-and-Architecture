module Andromeda.Hardware.Impl.Runtime where

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl

import qualified Data.Map as Map



type Devices = Map.Map T.ControllerName (TImpl.ControllerImpl, TImpl.Device)
