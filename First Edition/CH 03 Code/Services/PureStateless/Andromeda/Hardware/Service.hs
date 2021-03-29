module Andromeda.Hardware.Service where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.Device
import Andromeda.Hardware.Runtime

data Handle = Handle {
        createDevice :: Hdl () -> Device,
        getBlankDevice :: Device
    }

newHandle :: (Hdl () -> Device) -> Device -> Handle
newHandle mkDeviceF getBlankF = Handle mkDeviceF getBlankF

defaultHandle :: Handle
defaultHandle = newHandle makeDevice blankDevice
