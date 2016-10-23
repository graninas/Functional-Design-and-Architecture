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

updateSensorsModel :: SimulationModel -> SensorsModel
updateSensorsModel simModel =
    let oldSensors = sensorsModel simModel
        newSensors = M.map updateValue oldSensors
    in newSensors

updateControllersModel :: SimulationModel -> ControllersModel
updateControllersModel simModel =
    let oldControllers = controllersModel simModel
        newControllers = M.map updateLog oldControllers
    in newControllers

updateTerminalUnitsModel :: SimulationModel -> TerminalUnitModel
updateTerminalUnitsModel simModel =
    let oldTerminalUnits = terminalUnitsModel simModel
        newTerminalUnits = M.map updateUnit oldTerminalUnits
    in newTerminalUnits

updateSimulationModel :: SimulationModel -> SimulationModel
updateSimulationModel simModel =
    let newSensors = updateSensorsModel simModel
        newControllers = updateControllersModel simModel
        newTerminalUnits = updateTerminalUnitsModel simModel
    in SimulationModel newSensors newControllers newTerminalUnits
