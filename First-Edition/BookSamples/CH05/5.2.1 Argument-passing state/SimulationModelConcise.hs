module Andromeda.Simulator.SimulationModel where

import Data.Map as M

type PhysicalAddress = String
type ComponentIndex = String
type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

data ControllerNode = ControllerNode
data SensorNode = SensorNode
data TerminalUnitNode = TerminalUnitNode

type SensorsModel = M.Map ComponentInstanceIndex SensorNode
type ControllersModel = M.Map ComponentInstanceIndex ControllerNode
type TerminalUnitsModel = M.Map PhysicalAddress TerminalUnitNode

data SimulationModel = SimulationModel
    { sensorsModel :: SensorsModel
    , controllersModel :: ControllersModel
    , terminalUnitsModel :: TerminalUnitsModel
    }
    
emptySimModel = SimulationModel M.empty M.empty M.empty

updateValue :: SensorNode -> SensorNode
updateValue = undefined

updateLog :: ControllerNode -> ControllerNode
updateLog = undefined

updateUnit :: TerminalUnitNode -> TerminalUnitNode
updateUnit = undefined

updateSimulationModel :: SimulationModel -> SimulationModel
updateSimulationModel m = m
    { sensorsModel = M.map updateValue . sensorsModel $ m
    , controllersModel = M.map updateLog . controllersModel $ m
    , terminalUnitsModel = M.map updateUnit . terminalUnitsModel $ m
    }