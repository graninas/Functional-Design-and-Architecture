module Andromeda.Simulator.SimulationModel where

import Andromeda.Hardware

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forever, void)
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe
import Data.IORef
import Data.Traversable as T (mapM)


data ValueGenerator = NoGenerator
                    | StepGenerator (Measurement -> Measurement)

type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

-- Dummy types
data ControllerNode = ControllerNode
data TerminalUnitNode = TerminalUnitNode
type ControllersModel = M.Map ComponentInstanceIndex ControllerNode
type TerminalUnitsModel = M.Map PhysicalAddress TerminalUnitNode


data SensorNode = SensorNode
    { value :: Measurement
    , valueGenerator :: ValueGenerator
    , producing :: Bool
    }

type SensorNodeRef = IORef SensorNode
type SensorsModel = M.Map ComponentInstanceIndex SensorNodeRef

data SimulationModel = SimulationModel
    { sensorsModel :: SensorsModel
    , controllersModel :: ControllersModel
    , terminalUnitsModel :: TerminalUnitsModel
    }

emptySensorsModel = M.empty
emptyControllersModel = M.empty
emptyTerminalUnitsModel = M.empty
emptySimModel = SimulationModel M.empty M.empty M.empty

applyGenerator :: ValueGenerator -> Measurement -> Measurement
applyGenerator NoGenerator v = v
applyGenerator (StepGenerator f) v = f v

updateValue :: SensorNodeRef -> IO ()
updateValue nodeRef = do
    SensorNode val gen producing <- readIORef nodeRef
    when producing $ do
        let newVal = applyGenerator gen val
        let newNode = SensorNode newVal gen producing
        writeIORef nodeRef newNode
    threadDelay (1000 * 10)


type SensorHandle = (SensorNodeRef, ThreadId)
type SensorsHandles = M.Map ComponentInstanceIndex SensorHandle
    
forkSensorWorker :: SensorNodeRef -> IO SensorHandle
forkSensorWorker nodeRef = do
    threadId <- forkIO $ forever $ updateValue nodeRef
    return (nodeRef, threadId)
    
startSensorsSimulation :: SensorsModel -> IO SensorsHandles
startSensorsSimulation sensors = T.mapM forkSensorWorker sensors

stopSensorWorker :: SensorHandle -> IO ()
stopSensorWorker (_, threadId) = killThread threadId

stopSensorsSimulation :: SensorsHandles -> IO ()
stopSensorsSimulation handles = void $ T.mapM stopSensorWorker handles

readSensorNodeValue 
    :: ComponentInstanceIndex 
    -> SensorsHandles
    -> IO Measurement
readSensorNodeValue idx handles = case M.lookup idx handles of
    Just (nodeRef, _) -> do
        SensorNode val _ _ <- readIORef nodeRef
        return val
    Nothing -> do
        stopSensorsSimulation handles
        error $ "Index not found: " ++ show idx
