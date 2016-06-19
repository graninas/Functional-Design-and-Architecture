module DeviceTest where

import Andromeda.Hardware.Service
import TestData

testDevice :: Handle -> IO ()
testDevice h  = do
    blank    <- getBlankDevice h
    notBlank <- createDevice h boostersDef
    if (blank == notBlank)
        then putStrLn "FAILED"
        else putStrLn "passed"

