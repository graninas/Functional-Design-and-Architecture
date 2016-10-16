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
type TerminalUnitModel = M.Map PhysicalAddress TerminalUnitNode

data SimulationModel = SimulationModel
    { sensorsModel :: SensorsModel
    , controllersModel :: ControllersModel
    , terminalUnitsModel :: TerminalUnitModel
    }
    
emptySimModel = SimulationModel M.empty M.empty M.empty

updateValue :: SensorNode -> SensorNode
updateValue = undefined

updateLog :: ControllerNode -> ControllerNode
updateLog = undefined

updateUnit :: TerminalUnitNode -> TerminalUnitNode
updateUnit = undefined

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
        newTerminalUnits = M.map updateUnit oldTerminalUnits
    in simModel {terminalUnitsModel = newTerminalUnits}

updateSimulationModel :: SimulationModel -> SimulationModel
updateSimulationModel simModel =
    let simModel'   = updateSensorsModel simModel
        simModel''  = updateControllersModel simModel'
        simModel''' = updateTerminalUnitsModel simModel''
    in simModel'''
