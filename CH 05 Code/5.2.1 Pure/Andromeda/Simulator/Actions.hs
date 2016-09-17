{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Andromeda.Simulator.Actions where

import Andromeda.Common
import Andromeda.Hardware
import Andromeda.Calculations
import Andromeda.LogicControl
import Andromeda.Simulator.SimulationModel
import Andromeda.Simulator.HardwareHandle

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

-- TODO: static type checking.
fUpGen   (Par (FloatValue v) t) = Par (FloatValue (v + 1.0)) t
fDownGen (Par (FloatValue v) t) = Par (FloatValue (v - 1.0)) t

floatIncrementGen = StepGenerator fUpGen
floatDecrementGen = StepGenerator fDownGen

getSensorNode :: ComponentInstanceIndex -> SimState SensorNode
getSensorNode idx = do
    mbSensor <- use $ sensorsModel . at idx
    assert (isJust mbSensor) "Sensor not found" idx
    return $ fromJust mbSensor

setValueGenerator :: ComponentInstanceIndex -> ValueGenerator -> SimState ()
setValueGenerator idx g = do
    sensor <- getSensorNode idx
    let setValueGen tv g = liftIO $ atomically $ writeTVar tv g
    setValueGen (sensor ^. valueGenerator) g

setEnabled tmv = liftIO $ atomically $ putTMVar tmv True

runNetwork :: SimState ()
runNetwork = do
    m <- use $ sensorsModel
    let tmvs = m ^.. traverse . producing
    mapM_ setEnabled tmvs

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
