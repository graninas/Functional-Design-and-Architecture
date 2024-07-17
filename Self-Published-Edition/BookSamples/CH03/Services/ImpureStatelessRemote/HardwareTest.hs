module HardwareTest where

import Andromeda.Hardware.Device
import Andromeda.Hardware.Service
import Andromeda.Hardware.RemoteService
import Andromeda.Common.Pipe
import DeviceTest

import Control.Concurrent.MVar
import Control.Concurrent

mockedHandle = newHandle mock1 mock2
  where
    mock1 _ = do
        putStr " (createDevice mockedHandle) "
        return blankDevice
    mock2 = do
        putStr " (getBlankDevice mockedHandle) "
        return blankDevice

remoteHandle pipe = newRemoteHandle pipe

-- TODO: concurrent logging.
test = do
    pipe <- createPipe
    forkIO $ serviceWorker pipe

    putStr "With real service: "
    testDevice (remoteHandle pipe)
    
    putStr "With mocked service: "
    testDevice mockedHandle
    
    
    -- how to createDevice without passing handle but with ability to exchange service impl?
    -- IORef
    -- State
    
    
