module Andromeda.Simulator.SimulationModel where

import Data.Map as M
import Control.Monad.State

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

type SimState a = State SimulationModel a

updateSensors :: SimState SensorsModel
updateSensors = do
    sensors <- gets sensorsModel
    return $ M.map updateValue sensors
    
updateControllers :: SimState ControllersModel
updateControllers = do
    controllers <- gets controllersModel
    return $ M.map updateLog controllers

updateUnits :: SimState TerminalUnitModel
updateUnits = do
    units <- gets terminalUnitsModel
    return $ M.map updateUnit units
    
updateSimulationModelState :: SimState ()
updateSimulationModelState = do
    ss <- updateSensors
    cs <- updateControllers
    us <- updateUnits
    put $ SimulationModel ss cs us
    
updateSimulationModel :: SimulationModel -> SimulationModel
updateSimulationModel m = evalState act m
  where
    act :: SimState SimulationModel
    act = do
        ss <- updateSensors
        cs <- updateControllers
        us <- updateUnits
        return $ SimulationModel ss cs us
    
updateSimulationModel' :: SimulationModel -> SimulationModel
updateSimulationModel' = execState act
  where
    act :: SimState ()
    act = update >>= put
    update :: SimState SimulationModel
    update = liftM3 SimulationModel updateSensors updateControllers updateUnits
    
updateSimulationModelState' :: SimState ()
updateSimulationModelState' = put =<<
    liftM3 SimulationModel updateSensors updateControllers updateUnits

