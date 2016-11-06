{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Andromeda.Simulator.HardwareHandle where

import Andromeda.LogicControl
import Andromeda.Hardware
import Andromeda.Common
import Andromeda.Calculations
import Andromeda.Simulator.Simulation
import Andromeda.Simulator.SimulationModel

data HardwareHandle = HardwareHandle {
    hardwareRead :: forall tag . Controller -> ComponentIndex -> Parameter tag -> IO (Measurement tag)
}


