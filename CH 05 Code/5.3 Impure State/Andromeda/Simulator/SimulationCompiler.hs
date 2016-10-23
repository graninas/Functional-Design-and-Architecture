{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.SimulationCompiler where

import Andromeda.Hardware
import Andromeda.Simulator.SimulationModel

import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Free
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe
import Data.IORef

compileTerminalUnit
    :: SimulationModel
    -> PhysicalAddress
    -> IO (SimulationModel, TerminalUnitInterface)
compileTerminalUnit model addr = return (model, TerminalUnitInterface addr)

compileDeviceComponent addr (ss, cs) (SensorDef _ compIdx _ next) = do
    valIO <- newIORef (Measurement (FloatValue 1.0))
    valGenIO <- newIORef NoGenerator
    prodIO <- newIORef False
    let node = SensorNode valIO valGenIO prodIO
    let ss' = M.insert (addr, compIdx) node ss
    return (ss', cs, next)
compileDeviceComponent addr (ss, cs) (ControllerDef _ compIdx next) = do
    let cs' = M.insert (addr, compIdx) ControllerNode cs
    return (ss, cs', next)
    
compileHdl addr scs (Pure _) = return scs
compileHdl addr scs (Free component) = do
    (ss, cs, next) <- compileDeviceComponent addr scs component
    compileHdl addr (ss, cs) next

compileDevice
    :: SimulationModel
    -> PhysicalAddress
    -> Hdl ()
    -> IO (SimulationModel, DeviceInterface)
compileDevice (SimulationModel ss cs ts) addr hdl = do
    (ss', cs') <- compileHdl addr (ss, cs) hdl
    let model = SimulationModel ss' cs' ts
    return (model, DeviceInterface addr)

compileNetworkComponent 
    :: SimulationModel -> NetworkComponent (Hndl ()) -> IO SimulationModel
compileNetworkComponent model (DeviceDef addr hdl next) = do
    (model', iface) <- compileDevice model addr hdl
    compileHndl model' (next iface)
compileNetworkComponent model (TerminalUnitDef addr next) = do
    (model', iface) <- compileTerminalUnit model addr
    compileHndl model' (next iface)
compileNetworkComponent model (LogicControlDef addr next) =
    compileHndl model (next (LogicControlInterface addr))
compileNetworkComponent model (LinkedDeviceDef _ _ next) =
    compileHndl model next
compileNetworkComponent model (LinkDef _ _ next) =
    compileHndl model next
    
compileHndl :: SimulationModel -> Hndl () -> IO SimulationModel
compileHndl model (Pure _) = return model
compileHndl model (Free component) = compileNetworkComponent model component

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = compileHndl emptySimModel hndl
    

-- TODO: compiler
{-
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
    -}
