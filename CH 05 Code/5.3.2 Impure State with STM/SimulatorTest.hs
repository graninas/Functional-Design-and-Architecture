module SimulatorTest where

import Andromeda.Hardware
import Andromeda.Simulator
import Andromeda.Service

import SampleNetwork

-- TODO: type-level tricks to make float and measurement consistent.
increaseValue :: Float -> Measurement -> Measurement
increaseValue n (Measurement (FloatValue v)) = Measurement (FloatValue (v + n))

incrementGenerator :: ValueGenerator
incrementGenerator = StepGenerator (increaseValue 1.0)

test = do
    simulationModel <- compileSimModel networkDef
    (simulatorHandle, pipe) <- startSimulator simulationModel
    
    sendRequest pipe StartNetwork
    sendRequest pipe (SetGenerator boostersNozzle1T incrementGenerator)
    sendRequest pipe StopNetwork
    --res2 <- sendRequest pipe StartNetwork
    --res3 <- sendRequest pipe StopNetwork
    stopSimulator simulatorHandle
