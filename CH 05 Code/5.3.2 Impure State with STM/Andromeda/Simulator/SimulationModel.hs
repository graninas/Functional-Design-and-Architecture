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
    { value :: TVar Measurement
    , valueGenerator :: TVar ValueGenerator
    , producing :: TVar Bool
    }

type SensorsModel = M.Map ComponentInstanceIndex SensorNode

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

updateValueTrans :: SensorNode -> STM ()
updateValueTrans (SensorNode tvVal tvGen tvProduce) = do
    produce <- readTVar tvProduce
    when (not produce) retry
    val <- readTVar tvVal
    gen <- readTVar tvGen
    let newVal = applyGenerator gen val
    writeTVar tvVal newVal
    
updateValue :: SensorNode -> IO ()
updateValue node = do
    atomically $ updateValueTrans node
    threadDelay (1000 * 10) -- 10 ms

type SensorHandle = (SensorNode, ThreadId)
type SensorsHandles = M.Map ComponentInstanceIndex SensorHandle

forkSensorWorker :: SensorNode -> IO SensorHandle
forkSensorWorker node = do
    threadId <- forkIO $ forever $ updateValue node
    return (node, threadId)
    
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
    Just (SensorNode tvVal _ _, _) -> atomically $ readTVar tvVal
    Nothing -> do
        stopSensorsSimulation handles
        error $ "Index not found: " ++ show idx
