{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Simulator.SimulationModel where

import Andromeda.Common
import Andromeda.Hardware

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe

data ValueGenerator = NoGenerator
                    | StepGenerator (Par -> Par)

-- TODO: use Device as Node?
data ControllerNode = ControllerNode

type ValueSource = TVar Par
data SensorNode = SensorNode
    { _valueSource :: ValueSource
    , _valueGenerator :: TVar ValueGenerator
    , _producing :: TMVar Bool
    }

type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

type SensorsModel     = M.Map ComponentInstanceIndex SensorNode
type ControllersModel = M.Map PhysicalAddress ControllerNode
type NetworkScheme    = M.Map String String -- TODO

data SimulationModel = SimulationModel
    { _sensorsModel :: SensorsModel
    , _controllersModel :: ControllersModel
    , _networkScheme :: NetworkScheme
    }
    
type SimState = S.StateT SimulationModel IO
type Process req resp = req -> SimState resp

makeLenses ''SensorNode
makeLenses ''SimulationModel

emptySimModel = SimulationModel M.empty M.empty M.empty

noGenerator = NoGenerator
