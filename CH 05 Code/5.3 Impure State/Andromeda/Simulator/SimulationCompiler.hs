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

-- Simple compiler that doesn't use Interpreter type classes for HDL and HNDL.
-- To see the compiler that does, see "Impure State with IORef" sample.

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