module Andromeda.Hardware.Runtime (
    makeDevice,
    makeDeviceIO
) where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.Device

import Control.Monad.Free
import Control.Monad.State

interpretFreeSt :: Hdl () -> State Device ()
interpretFreeSt (Pure _)    = return ()
interpretFreeSt (Free comp) = interpretComponentSt comp

interpretComponentSt :: Component (Hdl ()) -> State Device ()
interpretComponentSt (SensorDef c idx par next) = do
    modify (addSensor idx par c)
    interpretFreeSt next
interpretComponentSt (ControllerDef c idx next) = do
    modify (addController idx c)
    interpretFreeSt next

makeDeviceSt :: Hdl () -> State Device ()
makeDeviceSt hdl = interpretFreeSt hdl

interpretFree :: Device -> Hdl () -> Device
interpretFree device (Pure _)    = device
interpretFree device (Free comp) = interpretComponent device comp

interpretComponent :: Device -> Component (Hdl ()) -> Device
interpretComponent device (SensorDef c idx par next) =
    let device' = addSensor idx par c device
    in interpretFree device' next
interpretComponent device (ControllerDef c idx next) =
    let device' = addController idx c device
    in interpretFree device' next

makeDevice :: Hdl () -> Device
makeDevice hdl = interpretFree blankDevice hdl

makeDeviceIO :: Hdl () -> IO Device
makeDeviceIO hdl = return (makeDevice hdl)

