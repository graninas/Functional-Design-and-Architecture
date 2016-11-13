{-# LANGUAGE TemplateHaskell #-}
module Andromeda.Simulator.Runtime where

import Andromeda.Simulator.Simulation
import Andromeda.Service

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Time.Clock
import Data.Maybe

data SimulatorHandle = SimulatorHandle
    { shSimulationModel :: SimulationModel
    , shSensorsHandles :: SensorsHandles
    , shStartTime :: UTCTime
    , shThreadId :: ThreadId
    }

data In = StartNetwork
        | StopNetwork
        | SetGenerator ComponentInstanceIndex ValueGenerator
data Out = Ok | Fail String
  deriving Eq
  
type SimulatorPipe = Pipe In Out

type SimulatorState = S.StateT SimulationModel IO
type Process = In -> SimulatorState Out

process :: Process
process StartNetwork = do
    liftIO $ print "Starting network..."
    startNetwork
    return Ok
process StopNetwork = do
    liftIO $ print "Stoping network..."
    stopNetwork
    return Ok
process (SetGenerator idx gen) = do
    liftIO $ print "Seting value generator..."
    setGenerator idx gen
    return Ok

processor :: SimulatorPipe -> SimulatorState ()
processor pipe = do
    req <- liftIO $ getRequest pipe
    resp <- process req
    liftIO $ sendResponse pipe resp

forkSimulatorWorker :: SimulationModel -> SimulatorPipe -> IO ThreadId
forkSimulatorWorker simModel pipe = do
    let simulatorState = forever $ processor pipe
    forkIO $ void $ S.execStateT simulatorState simModel
    
startSimulator :: SimulationModel -> IO (SimulatorHandle, SimulatorPipe)
startSimulator simModel@(SimulationModel sensorsModel _ _) = do
    pipe <- createPipe :: IO SimulatorPipe
    
    startTime <- getCurrentTime
    sensorsHandles <- startSensorsSimulation sensorsModel
    threadId <- forkSimulatorWorker simModel pipe
        
    let handle = SimulatorHandle simModel sensorsHandles startTime threadId
    return (handle, pipe)

stopSimulator :: SimulatorHandle -> IO ()
stopSimulator (SimulatorHandle _ sensorsHandles _ threadId) = do
    stopSensorsSimulation sensorsHandles
    killThread threadId

startNetwork :: SimulatorState ()
startNetwork = return ()

stopNetwork :: SimulatorState ()
stopNetwork = return ()

setGenerator :: ComponentInstanceIndex -> ValueGenerator -> SimulatorState ()
setGenerator idx gen = undefined

{-
setEnabled enabled tmv = liftIO $ atomically $ putTMVar tmv enabled

startNetwork :: SimulatorState ()
startNetwork = do
    m <- getSensorsModel
    let tmvs = m ^.. traverse . producing
    mapM_ (setEnabled True) tmvs

stopNetwork :: SimulatorState ()
stopNetwork = do
    m <- use $ sensorsModel
    let tmvs = m ^.. traverse . producing
    mapM_ (setEnabled False) tmvs
    -}

-- Actions
{-
getSensorNode :: ComponentInstanceIndex -> SimulatorState SensorNode
getSensorNode idx = do
    mbSensor <- use $ sensorsModel . at idx
    when (not $ isJust mbSensor) $ error "Sensor not found."
    return $ fromJust mbSensor

setValueGenerator
    :: ComponentInstanceIndex -> ValueGenerator -> SimulatorState ()
setValueGenerator idx g = do
    sensor <- getSensorNode idx
    let setValueGen tv g = liftIO $ atomically $ writeTVar tv g
    setValueGen (sensor ^. valueGenerator) g
    -}
    
{-
-- TODO
   
getValueSource :: ComponentInstanceIndex -> SimState ValueSource
getValueSource idx = do
    sensor <- getSensorNode idx
    return $ sensor ^. valueSource
    
getValueSources :: SimState (M.Map ComponentInstanceIndex ValueSource)
getValueSources = do
    model <- use sensorsModel
    return $ M.map (\sensor -> sensor ^. valueSource) model
    
readValueSource :: ValueSource -> IO Par
readValueSource vs = liftIO $ atomically $ readTVar vs

getHardwareHandle :: SimState HardwareHandle
getHardwareHandle = do
    vss <- getValueSources
    return $ HardwareHandle (readF vss)
  where
    readF valueSources (Controller addr) ci _ = do
        let mbVs = valueSources ^. at (addr, ci)
        assert (isJust mbVs) "Component not found" (addr, ci)
        parVal <- readValueSource (fromJust mbVs)
        return $ toMeasurement parVal
        -}