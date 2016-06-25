{-# LANGUAGE Arrows #-}
module ArrowDSL where

import Control.Arrow
import Control.Category
import Prelude hiding ((.))

import ControllerDSL as C
import InfrastructureDSL as I
import ScriptingDSL as S
import Control

import ArrEff

-- arrow type
type FlowArr b c = ArrEffFree Control b c
data ValidationResult = Success
                      | Failure String

-- This is a dummy arrow to demonstrate there can be some effectful arrows instead this one.
getTimeA :: FlowArr b Time
getTimeA = aConst 4
-- This is a demo arrow. It expects temperature in Celsius.
toKelvinA :: FlowArr Measurement Measurement
toKelvinA = arr toKelvin

thermMonitorA :: FlowArr SensorInstance Reading
thermMonitorA = 
      duplicateA                               -- (inst, inst)
  >>> second (thermTemperatureA >>> toKelvinA) -- (inst, tempK)
  >>> (arr $ \x -> ((), x))                    -- ((), (inst, tempK))
  >>> first getTimeA                           -- (time, (inst, tempK))
  >>> (arr $ \(t, (inst, m)) -> (t, inst, m))  -- (time, inst, tempK) = reading
  >>> duplicateA                               -- (reading, reading)
  >>> second (storeReadingA &&& validateReadingA) -- (reading, ((), result))
  >>> second (second alarmOnFailA)                -- (reading, ((), ()))
  >>> takeFirstA

thermMonitorA' :: FlowArr SensorInstance Reading
thermMonitorA' = proc sensorInst -> do
    tempK <- toKelvinA <<< thermTemperatureA -< sensorInst
    time  <- getTimeA -< ()
    
    let reading = (time, sensorInst, tempK)
    
    ()      <- storeReadingA    -< reading
    result  <- validateReadingA -< reading
    ()      <- alarmOnFailA     -< result
    returnA -< reading

readSensor :: Parameter -> SensorInstance -> Script Measurement
readSensor p (cont, idx) = controllerScript $ C.read cont idx p

alarmOnFail :: ValidationResult -> Script ()
alarmOnFail Success       = infrastructureScript $ return ()
alarmOnFail (Failure msg) = infrastructureScript $ alarm msg

validateReading :: Reading -> ValidationResult
validateReading (_, si, Measurement (FloatValue tempK)) =
    if (tempK < 263.15)
    then Failure (show (si, "Temperature lower than bound"))
    else if (tempK > 323.15)
         then Failure (show (si, "Temperature higher than bound"))
         else Success

thermTemperatureA :: FlowArr SensorInstance Measurement
thermTemperatureA = mArr (evalScript . readSensor temperature)

storeReadingA :: FlowArr Reading ()
storeReadingA = mArr (evalScript . infrastructureScript . storeReading)

validateReadingA :: FlowArr Reading ValidationResult
validateReadingA = arr validateReading

alarmOnFailA :: FlowArr ValidationResult ()
alarmOnFailA = mArr (evalScript . alarmOnFail)


test = do
    
    
    
    print ""