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

readSensor :: Parameter -> SensorInstance -> Script Measurement
readSensor p (cont, idx) = controllerScript readSensor'
  where
      readSensor' :: ControllerScript Measurement
      readSensor' = C.read cont idx p

alarmOnFail :: ValidationResult -> Script ()
alarmOnFail Success       = infrastructureScript $ return ()
alarmOnFail (Failure msg) = infrastructureScript $ alarm msg

thermTemperatureA :: FlowArr SensorInstance Measurement
thermTemperatureA = mArr (evalScript . readSensor temperature)

alarmOnFailA :: FlowArr ValidationResult ()
alarmOnFailA = mArr (evalScript . alarmOnFail)

storeReadingA :: FlowArr Reading ()
storeReadingA = mArr (evalScript . infrastructureScript . storeReading)

getTimeA :: FlowArr b Time
getTimeA = mArr (evalScript . infrastructureScript . const getCurrentTime)

toKelvinA :: FlowArr Measurement Measurement
toKelvinA = arr toKelvin

validateReadingA :: FlowArr Reading ValidationResult
validateReadingA = arr validateReading

validateReading :: Reading -> ValidationResult
validateReading (_, si, Measurement (FloatValue tempK)) =
    if (tempK < 263.15)
    then Failure (show (si, "Temperature lower than bound"))
    else if (tempK > 323.15)
         then Failure (show (si, "Temperature higher than bound"))
         else Success


    
