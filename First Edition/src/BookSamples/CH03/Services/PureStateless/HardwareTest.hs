module HardwareTest where

import Andromeda.Hardware.Device
import Andromeda.Hardware.Service
import DeviceTest

makeDeviceMock _ = blankDevice
mockedHandle = newHandle makeDeviceMock blankDevice

test = do
    putStr "With real service: "
    testDevice defaultHandle
    
    putStr "With mocked service: "
    testDevice mockedHandle
    
    
    -- how to createDevice without passing handle but with ability to exchange service impl?
    -- IORef
    -- State
    
    
