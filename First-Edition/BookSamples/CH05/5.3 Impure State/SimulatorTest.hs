module SimulatorTest where

import Andromeda.Hardware
import Andromeda.Simulator
import SampleNetwork
import Control.Concurrent

    

test = do
    simModel <- compileSimModel networkDef
    getLine

{-
-- TODO
test = do
    simModel <- compileSimModel networkDef
    return ()
    
    r1 <- sendRequest pipe (setGen1Act boostersNozzle1T)
    r2 <- sendRequest pipe runNetworkAct
    (OutValueSource vs) <- sendRequest pipe (GetValueSource boostersNozzle1T)
    vals <- sequence (replicate 10 $ threadDelay 1000 >> readValueSource vs)
    stopSimulation simHandle
-}