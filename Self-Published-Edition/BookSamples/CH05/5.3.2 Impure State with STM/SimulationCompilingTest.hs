module SimulationCompilingTest where

import Andromeda.Hardware
import Andromeda.Simulator
import SampleNetwork


test = do
    SimulationModel sensors _ _ <- compileSimModel networkDef
    handles <- startSensorsSimulation sensors
    value1 <- readSensorNodeValue ("01", "nozzle1-t") handles
    value2 <- readSensorNodeValue ("01", "nozzle2-t") handles
    print value1
    print value2
    stopSensorsSimulation handles
    
    
    