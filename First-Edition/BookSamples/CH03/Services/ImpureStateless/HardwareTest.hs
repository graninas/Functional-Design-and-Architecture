module HardwareTest where

import Andromeda.Hardware.Device
import Andromeda.Hardware.Service
import DeviceTest

mockedHandle = newHandle mock1 mock2
  where
    mock1 _ = do
        putStr " (createDevice mockedHandle) "
        return blankDevice
    mock2 = do
        putStr " (getBlankDevice mockedHandle) "
        return blankDevice
        
test = do
    putStr "With real service: "
    testDevice defaultHandle
    
    putStr "With mocked service: "
    testDevice mockedHandle
    
    
    -- how to createDevice without passing handle but with ability to exchange service impl?
    -- IORef
    -- State
    
    
