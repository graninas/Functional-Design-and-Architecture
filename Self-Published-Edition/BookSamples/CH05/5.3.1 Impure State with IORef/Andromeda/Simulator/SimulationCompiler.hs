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


data CompilerState = CompilerState
    { _currentPhysicalAddress :: PhysicalAddress
    , _composingSensors :: SensorsModel
    , _composingControllers :: ControllersModel
    , _composingTerminalUnits :: TerminalUnitsModel
    }

makeLenses ''CompilerState

type SimCompilerState = S.StateT CompilerState IO

compileSensorNode :: Parameter -> SimCompilerState SensorNodeRef
compileSensorNode par = do
    let node = SensorNode (toMeasurement par) NoGenerator False
    liftIO $ newIORef node

{-
-- Compilation with lenses example
instance HdlInterpreter SimCompilerState where
   onSensorDef compDef compIdx par = do
       node <- compileSensorNode par
       addr <- use currentPhysicalAddress
       sensors <- use composingSensors
       composingSensors .= M.insert (addr, compIdx) node sensors
   onControllerDef compDef compIdx = do
       return () -- not implemented
-}

instance HdlInterpreter SimCompilerState where
    onSensorDef compDef compIdx par = do
        node <- compileSensorNode par
--       CompilerState addr ss cs ts  <- S.get
--       let newSensors = M.insert (addr, compIdx) node ss
--       S.put $ CompilerState addr newSensors cs ts
        addr <- use currentPhysicalAddress
        let appendToMap = M.insert (addr, compIdx) node
        composingSensors %= appendToMap
    onControllerDef compDef compIdx =
        return () -- not implemented
      
instance HndlInterpreter SimCompilerState where
    onDeviceDef addr hdl = do
        currentPhysicalAddress .= addr
        interpretHdl hdl
        return $ mkDeviceInterface addr
    onTerminalUnitDef addr = 
        return $ mkTerminalUnitInterface addr
    onLogicControlDef addr = 
        return $ mkLogicControlInterface addr
    onLinkedDeviceDef _ _ = return () -- not implemented
    onLinkDef _ _ = return () -- not implemented


---- public interface:

compileSimModel :: Hndl () -> IO SimulationModel
compileSimModel hndl = do
    let compiler = interpretHndl hndl
    let state = CompilerState "" M.empty M.empty M.empty
    (CompilerState _ ss cs ts) <- S.execStateT compiler state
    return $ SimulationModel ss cs ts