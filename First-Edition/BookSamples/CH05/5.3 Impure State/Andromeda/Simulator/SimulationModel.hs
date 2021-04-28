module Andromeda.Simulator.SimulationModel where

import Andromeda.Hardware

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Maybe
import Data.IORef

data ValueGenerator = NoGenerator
                    | StepGenerator (Measurement -> Measurement)

type ComponentInstanceIndex = (PhysicalAddress, ComponentIndex)

-- Dummy types
data ControllerNode = ControllerNode
data TerminalUnitNode = TerminalUnitNode
type ControllersModel = M.Map ComponentInstanceIndex ControllerNode
type TerminalUnitModel = M.Map PhysicalAddress TerminalUnitNode


data SensorNode = SensorNode
    { value :: IORef Measurement
    , valueGenerator :: IORef ValueGenerator
    , producing :: IORef Bool
    }

type SensorsModel = M.Map ComponentInstanceIndex SensorNode

data SimulationModel = SimulationModel
    { sensorsModel :: SensorsModel
    , controllersModel :: ControllersModel
    , terminalUnitsModel :: TerminalUnitModel
    }

emptySensorsModel = M.empty
emptyControllersModel = M.empty
emptyTerminalUnitsModel = M.empty
emptySimModel = SimulationModel M.empty M.empty M.empty

applyGenerator :: ValueGenerator -> Measurement -> Measurement
applyGenerator NoGenerator v = v
applyGenerator (StepGenerator f) v = f v

updateValue :: SensorNode -> IO ()
updateValue node@(SensorNode valIORef genIORef producingIORef) = do
    isProducing <- readIORef producingIORef
    if isProducing 
        then do
            gen <- readIORef genIORef
            val <- readIORef valIORef
            let newVal = applyGenerator gen val
            writeIORef valIORef newVal
        else return ()


