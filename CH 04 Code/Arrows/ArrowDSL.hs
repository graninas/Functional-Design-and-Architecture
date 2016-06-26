{-# LANGUAGE Arrows #-}
module ArrowDSL where

import Control.Arrow
import Control.Category
import Control.Monad.Free
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

thermMonitorA' :: FlowArr SensorInstance Reading
thermMonitorA' = 
      duplicateA                               -- (inst, inst)
  >>> second (thermTemperatureA >>> toKelvinA) -- (inst, tempK)
  >>> (arr $ \x -> ((), x))                    -- ((), (inst, tempK))
  >>> first getTimeA                           -- (time, (inst, tempK))
  >>> (arr $ \(t, (inst, m)) -> (t, inst, m))  -- (time, inst, tempK) = reading
  >>> duplicateA                               -- (reading, reading)
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

interpretControlProgram :: ControlProgram a -> IO a
interpretControlProgram (Pure a) = return a
interpretControlProgram (Free (EvalScript scr next)) = do
    res <- interpretScript scr
    interpretControlProgram (next res)

interpretScript (ControllerScript scr) = interpretControllerScript scr
interpretScript (InfrastructureScript scr) = interpretInfrastructureScript scr

interpretControllerScript (Pure a) = return a
interpretControllerScript (Free (Get c p next)) = do
    print ("Get", c, p)
    interpretControllerScript (next (StringValue "ggg"))
interpretControllerScript (Free (Set c p v next)) = do
    print ("Get", c, p, v)
    interpretControllerScript next
interpretControllerScript (Free (Read c si p next)) = do
    print ("Read", c, si, p)
    interpretControllerScript (next (Measurement . FloatValue $ 33.3))
interpretControllerScript (Free (Run c cmd next)) = do
    print ("Run", c, cmd)
    interpretControllerScript (next (Right "OK."))

interpretInfrastructureScript (Pure a) = return a
interpretInfrastructureScript (Free (StoreReading r next)) = do
    print ("StoreReading", r)
    interpretInfrastructureScript next
interpretInfrastructureScript (Free (SendTo r v next)) = do
    print ("SendTo", v)
    r v
    interpretInfrastructureScript next

test = do
    let sensorInst = (Controller "boosters", "00:01")
    runFreeArr interpretControlProgram thermMonitorA sensorInst
    
    print ""