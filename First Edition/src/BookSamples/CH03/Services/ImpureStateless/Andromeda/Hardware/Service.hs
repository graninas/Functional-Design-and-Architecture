module Andromeda.Hardware.Service where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.Device
import Andromeda.Hardware.Runtime

data Handle = Handle {
        createDevice :: Hdl () -> IO Device,
        getBlankDevice :: IO Device
    }

newHandle :: (Hdl () -> IO Device) -> IO Device -> Handle
newHandle mkDeviceF getBlankF = Handle mkDeviceF getBlankF

defaultHandle :: Handle
defaultHandle = newHandle mkDeviceF' getBlankF'
  where
    mkDeviceF' ::  Hdl () -> IO Device
    mkDeviceF' hdl = do
        putStr " (createDevice defaultHandle) "
        return (makeDevice hdl)
    getBlankF' :: IO Device
    getBlankF' = do
        putStr " (getBlankDevice defaultHandle) "
        return blankDevice

