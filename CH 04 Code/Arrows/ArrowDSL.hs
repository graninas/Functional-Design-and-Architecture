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

-- dummy types
type Time = Int
data ValidationResult = Success
                      | Failure String

type SensorInstance = (Controller, SensorIndex)
type Reading = (Time, SensorInstance, Measurement)

-- arrow type
type FlowArr b c = ArrEffFree Control b c


-- This is a dummy arrow to demonstrate there can be some effectful arrows instead this one.
getTimeA :: FlowArr b Time
getTimeA = aConst 4
-- This is a demo arrow. It expects temperature in Celsius.
toKelvinA :: FlowArr Measurement Measurement
toKelvinA = arr toKelvin

thermMonitorA :: FlowArr SensorInstance (Reading, String)
thermMonitorA = 
      duplicateA                               -- (inst, inst)
  >>> second (thermTemperatureA >>> toKelvinA) -- (inst, tempK)
  >>> (arr $ \x -> ((), x))                    -- ((), (inst, tempK))
  >>> first getTimeA                           -- (time, (inst, tempK))
  >>> (arr $ \(t, (inst, m)) -> (t, inst, m))  -- (time, inst, tempK) = reading
  >>> duplicateA                               -- (reading, reading)
                                               -- (reading, ((), result))        
  >>> second (storeInDatabaseA &&& validateTemperatureA)
  >>> second (second analyzeFailuresA)         -- (reading, ((), resStr))
  >>> (arr $ \(reading, (_, resStr)) -> (reading, resStr))

thermMonitorA' :: FlowArr SensorInstance (Reading, String)
thermMonitorA' = proc sensorInst -> do
    tempK <- toKelvinA <<< thermTemperatureA -< sensorInst
    time  <- getTimeA -< ()
    
    let reading = (time, sensorInst, tempK)
    
    ((), result) <- (storeInDatabaseA &&& validateTemperatureA) -< reading
    resStr       <- analyzeFailuresA -< result
    returnA      -< (reading, resStr)

readTemperatureScript :: SensorInstance -> Script Measurement
readTemperatureScript (cont, idx) = controllerScript $ C.read cont idx temperature

thermTemperatureA :: FlowArr SensorInstance Measurement
thermTemperatureA = mArr (evalScript . readTemperatureScript)

processTemperatureA :: FlowArr Reading ()
processTemperatureA = undefined

storeInDatabaseA :: FlowArr Reading ()
storeInDatabaseA = undefined

validateTemperatureA :: FlowArr Reading ValidationResult
validateTemperatureA = undefined

analyzeFailuresA :: FlowArr ValidationResult String
analyzeFailuresA = undefined



test = do
    
    
    print ""