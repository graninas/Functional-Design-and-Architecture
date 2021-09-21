module Andromeda.Hardware.Impl.HilInterpreter where

import qualified Andromeda.Hardware.Language.Hil as L

import qualified Andromeda.Hardware.Common as T
import qualified Andromeda.Hardware.Domain as T

import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Component as CImpl

-- This interpreter should not know about this module.
-- Everything is accessible through HardwareService.
-- import qualified Andromeda.Hardware.Impl.Device as Impl


import Data.IORef
import qualified Data.Map as Map
import Control.Monad.Free (foldFree)


getDevice :: RImpl.HardwareRuntime -> T.Controller -> IO (Maybe TImpl.Device)
getDevice RImpl.HardwareRuntime {devicesRef} ctrl = do
  devices <- readIORef devicesRef
  pure $ Map.lookup ctrl devices


interpretHilMethod :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.HilMethod a -> IO a

interpretHilMethod runtime service (L.GetStatus ctrl next) =
  -- TODO: dummy
  pure $ next $ Right T.StatusOk

interpretHilMethod runtime service (L.ReadSensor controller idx next) = do
  mbDevice <- getDevice runtime controller
  case mbDevice of
    Nothing -> pure $ next $ Left "Device not found"
    Just device -> do
      mbDevicePart <- SImpl.getDevicePart service idx device
      case mbDevicePart of
        Nothing -> pure $ next $ Left $ "Device part not found: " <> idx
        Just devicePart -> do
          measurement <- TImpl.withHandler devicePart $ \handler ->
            CImpl.readMeasurement handler
          pure $ next $ Right measurement

runHil :: RImpl.HardwareRuntime -> SImpl.HardwareService -> L.Hil a -> IO a
runHil runtime service hil = foldFree (interpretHilMethod runtime service) hil
