{-# LANGUAGE Arrows #-}
module TestArrows where

import Control.Arrow
import Control.Category
import Control.Monad.Free
import Prelude hiding ((.))
import ArrEff

import ControllerDSL as C
import InfrastructureDSL as I
import ScriptingDSL as S
import Control
import ArrowDSL
import Interpreters
import TestProgram hiding (test)

thermMonitorA' :: FlowArr SensorInstance Reading
thermMonitorA' = 
      duplicateA                                  -- (inst, inst)
  >>> second (thermTemperatureA >>> toKelvinA)    -- (inst, tempK)
  >>> (arr $ \x -> ((), x))                       -- ((), (inst, tempK))
  >>> first getTimeA                              -- (time, (inst, tempK))
  >>> (arr $ \(t, (inst, m)) -> (t, inst, m))     -- (time, inst, tempK) = reading
  >>> duplicateA                                  -- (reading, reading)
  >>> second (storeReadingA &&& validateReadingA) -- (reading, ((), result))
  >>> second (second alarmOnFailA)                -- (reading, ((), ()))
  >>> takeFirstA

thermMonitorA :: FlowArr SensorInstance Reading
thermMonitorA = proc sensorInst -> do
    tempK <- toKelvinA <<< thermTemperatureA -< sensorInst
    time  <- getTimeA -< ()
    
    let reading = (time, sensorInst, tempK)
    
    ()      <- storeReadingA    -< reading
    result  <- validateReadingA -< reading
    ()      <- alarmOnFailA     -< result
    returnA -< reading

controlProgramA = mArr (const controlProgram)
    
test = do
    let sensorInst = (Controller "boosters", "00:01")
    runFreeArr interpretDeviceControl thermMonitorA sensorInst
    runFreeArr interpretDeviceControl controlProgramA ()