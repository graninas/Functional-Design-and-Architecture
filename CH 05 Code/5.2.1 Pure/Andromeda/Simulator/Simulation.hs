{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Simulation
    ( compileSimModel
    , simulation
    , startSimulation
    , stopSimulation
    , SimulatorHandle
    ) where

import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Simulator.SimulationModel

import Control.Service.Remote

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

data ComposingDevice = ComposingDevice
    { _composingSensors :: M.Map ComponentIndex SensorNode
    , _composingController :: Maybe (ComponentIndex, ControllerNode)
    }

data CompilerState = CompilerState
    { _simulationModel :: SimulationModel
    , _debugPrintEnabled :: Bool
    , _composingDevice :: ComposingDevice
    }
    
makeLenses ''ComposingDevice
makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO

type SimulatorHandle = ThreadId

workerThreadsDelay = 10 -- ms

emptyComposingDevice = ComposingDevice M.empty Nothing
emptyCompilerState = CompilerState emptySimModel False emptyComposingDevice

assertNoSensor idx = do
    mbS <- use (composingDevice . composingSensors . at idx)
    assert (isNothing mbS) "Sensor exist" idx

assertNoController idx = do
    mbC <- use $ composingDevice . composingController
    assert (isNothing mbC) "Controller exist" idx
    
mkDefaultSensorNode p = do
    tvP <- liftIO $ newTVarIO p
    tvG <- liftIO $ newTVarIO noGenerator
    tmvProd <- liftIO $ newEmptyTMVarIO
    return $ SensorNode tvP tvG tmvProd
    
mkDefaultControllerNode = return ControllerNode

debugPrint v = do
    dp <- use debugPrintEnabled
    if dp then liftIO $ print v
          else return ()

waitProducing prodTMvar = do
    prod <- takeTMVar prodTMvar
    putTMVar prodTMvar prod
    return prod

generateValue NoGenerator vs = vs
generateValue (StepGenerator f) vs = f vs

processValueSource False _ _ = return ()
processValueSource True vsTVar vgTVar = do
    vs <- readTVar vsTVar
    vg <- readTVar vgTVar
    let vg' = generateValue vg vs
    writeTVar vsTVar vg'

sensorWorker sn@(SensorNode vsTVar vgTVar prodTMvar) = do
    prod <- liftIO $ atomically $ waitProducing prodTMvar
    liftIO $ atomically $ processValueSource prod vsTVar vgTVar
    threadDelay workerThreadsDelay
    sensorWorker sn

instance HdlInterpreter SimCompilerState where
   onSensorDef compDef compIdx par = do
       debugPrint ("Compiling SensorDef", compIdx, compDef)
       assertNoSensor compIdx
       sn <- mkDefaultSensorNode par
       liftIO $ forkIO $ sensorWorker sn
       composingDevice . composingSensors . at compIdx %= (const $ Just sn)
   onControllerDef compDef compIdx = do
       debugPrint ("Compiling ControllerDef", compIdx, compDef)
       assertNoController compIdx
       cn <- mkDefaultControllerNode
       composingDevice . composingController .= (Just (compIdx, cn))

instance HndlInterpreter SimCompilerState where
   onDeviceDef pa hdl d = do
       debugPrint ("Compiling DeviceDef", pa, d)
       interpretHdl hdl
       m <- use $ composingDevice . composingSensors
       let m' = M.mapKeys (\compIdx -> (pa, compIdx)) m
       simulationModel . sensorsModel %= (M.union m')
       composingDevice .= emptyComposingDevice
       return $ mkDeviceInterface pa
   onTerminalUnitDef pa d = do
       debugPrint ("Compiling TerminalUnitDef", pa)
       return $ mkTerminalUnitInterface pa
   onLogicControlDef pa d = do
       debugPrint ("Compiling LogicControlDef", pa)
       return $ mkInterface pa
   onLinkedDeviceDef (DeviceInterface rdi) (TerminalUnitInterface tui) = do
       debugPrint ("Compiling LinkedDeviceDef", rdi, tui)
       return ()
   onLinkDef interf tui = do
       debugPrint "Compiling ConnectionDef"
       return ()

---- public interface:

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = do
    let compiler = interpretHndl hndl
    (CompilerState m _ _) <- S.execStateT compiler emptyCompilerState
    return m
    
simulation :: Pipe req resp -> Process req resp -> SimState ()
simulation pipe process = do
    req <- liftIO $ getRequest pipe
    resp <- process req
    liftIO $ sendResponse pipe resp
    simulation pipe process

-- TODO: stop all child threads
startSimulation pipe process simModel = forkIO $ void $ S.execStateT (simulation pipe process) simModel
stopSimulation = killThread
