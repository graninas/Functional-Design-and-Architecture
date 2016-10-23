module Andromeda.Simulator.SimulationModel where

import Andromeda.Hardware

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

data ValueGenerator = NoGenerator
                    | StepGenerator (Measurement -> Measurement)
                    
instance Show ValueGenerator where
    show NoGenerator = "NoGenerator"
    show _ = ""
                    
type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

data ControllerNode = ControllerNode
  deriving Show
data SensorNode = SensorNode
    { value :: Measurement
    , valueGenerator :: ValueGenerator
    , producing :: Bool
    }
  deriving Show
data TerminalUnitNode = TerminalUnitNode

type SensorsModel     = M.Map ComponentInstanceIndex SensorNode
type ControllersModel = M.Map ComponentInstanceIndex ControllerNode
type TerminalUnitModel = M.Map PhysicalAddress TerminalUnitNode

data SimulationModel = SimulationModel
    { sensorsModel :: SensorsModel
    , controllersModel :: ControllersModel
    , terminalUnitsModel :: TerminalUnitModel
    }
    
emptySimModel = SimulationModel M.empty M.empty M.empty

applyGenerator :: ValueGenerator -> Measurement -> Measurement
applyGenerator NoGenerator v = v
applyGenerator (StepGenerator f) v = f v

updateValue :: SensorNode -> SensorNode
updateValue node@(SensorNode val gen True) =
    let newVal = applyGenerator gen val
    in SensorNode newVal gen True
updateValue node@(SensorNode val gen False) = node

updateLog :: ControllerNode -> ControllerNode
updateLog = undefined

-- TODO: rename this (in the book it's called updateUnit)
updateSomething :: TerminalUnitNode -> TerminalUnitNode
updateSomething = undefined

updateSensorsModel :: SimulationModel -> SimulationModel
updateSensorsModel simModel =
    let oldSensors = sensorsModel simModel
        newSensors = M.map updateValue oldSensors
    in simModel {sensorsModel = newSensors}

updateControllersModel :: SimulationModel -> SimulationModel
updateControllersModel simModel =
    let oldControllers = controllersModel simModel
        newControllers = M.map updateLog oldControllers
    in simModel {controllersModel = newControllers}

updateTerminalUnitsModel :: SimulationModel -> SimulationModel
updateTerminalUnitsModel simModel =
    let oldTerminalUnits = terminalUnitsModel simModel
        newTerminalUnits = M.map updateSomething oldTerminalUnits
    in simModel {terminalUnitsModel = newTerminalUnits}

updateSimulationModel :: SimulationModel -> SimulationModel
updateSimulationModel simModel =
    let simModel'   = updateSensorsModel simModel
        simModel''  = updateControllersModel simModel'
        simModel''' = updateTerminalUnitsModel simModel''
    in simModel'''

updateSimulationModel' :: SimulationModel -> SimulationModel
updateSimulationModel' simModel = simModel
    { sensorsModel = M.map updateValue . sensorsModel $ simModel
    , controllersModel = M.map updateLog . controllersModel $ simModel
    , terminalUnitsModel = M.map updateSomething . terminalUnitsModel $ simModel
    }
