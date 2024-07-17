module DeviceTest where

import Andromeda.Hardware.Service
import TestData

testDevice :: Handle -> IO ()
testDevice h  = do
    let blank    = getBlankDevice h
    let notBlank = createDevice h boostersDef
    if (blank == notBlank)
        then putStrLn "FAILED"
        else putStrLn "passed"

