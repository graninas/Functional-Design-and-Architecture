{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.Simulation where

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

compileTerminalUnit
    :: SimulationModel
    -> PhysicalAddress
    -> (SimulationModel, TerminalUnitInterface)
compileTerminalUnit model addr = (model, TerminalUnitInterface addr)

compileDevice 
    :: SimulationModel
    -> PhysicalAddress
    -> Hdl ()
    -> (SimulationModel, DeviceInterface)
compileDevice model addr hdl = (model, DeviceInterface addr)

compileNetworkComponent 
    :: SimulationModel -> NetworkComponent (Hndl ()) -> SimulationModel

compileNetworkComponent model (DeviceDef addr hdl next) =
    let (model', iface) = compileDevice model addr hdl
        nextHndl = next iface
    in compileHndl model' nextHndl
compileNetworkComponent model (TerminalUnitDef addr next) =
    let (model', iface) = compileTerminalUnit model addr
        nextHndl = next iface
    in compileHndl model' nextHndl
    
compileHndl :: SimulationModel -> Hndl () -> SimulationModel
compileHndl model (Pure _) = model
compileHndl model (Free component) = compileNetworkComponent model component

compileSimModel :: Hndl () -> SimulationModel
compileSimModel hndl = compileHndl emptySimModel hndl


