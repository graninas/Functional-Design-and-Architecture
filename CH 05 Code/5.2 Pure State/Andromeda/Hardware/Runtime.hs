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

{-
interpretPureStateless :: Hdl () -> Device
interpretPureStateless hdl = interpret' blankDevice hdl
  where
    interpret' :: Device -> Hdl () -> Device
    interpret' d (Pure _) = d
    interpret' d (Free proc) = case proc of
        SensorDef c idx par next ->
            let d' = addSensor idx par c d
            in interpret' d' next
        ControllerDef c idx next ->
            let d' = addController idx c d
            in interpret' d' next

interpretImpureStateless :: Hdl () -> IO Device
interpretImpureStateless hdl = interpret' blankDevice hdl
  where
    interpret' :: Device -> Hdl () -> IO Device
    interpret' d (Pure _) = return d
    interpret' d (Free proc) = case proc of
        SensorDef c idx par next -> do
            let d' = addSensor idx par c d
            print "Sensor added."
            interpret' d' next
        ControllerDef c idx next -> do
            let d' = addController idx c d
            print "Controller added."
            interpret' d' next

interpretPureStateful :: Hdl () -> State Device ()
interpretPureStateful hdl = interpret' hdl
  where
    interpret' :: Hdl () -> State Device ()
    interpret' (Pure _) = return ()
    interpret' (Free proc) = case proc of
        SensorDef c idx par next -> do
            modify (addSensor idx par c)
            interpret' next
        ControllerDef c idx next -> do
            modify (addController idx c)
            interpret' next

interpretImpureStateful :: Hdl () -> IO (IORef Device)
interpretImpureStateful hdl = do
    deviceIO <- newIORef blankDevice
    interpret' deviceIO hdl
  where
    interpret' :: IORef Device -> Hdl () -> IO (IORef Device)
    interpret' deviceIO (Pure _) = return deviceIO
    interpret' deviceIO (Free proc) = case proc of
        SensorDef c idx par next -> do
            modifyIORef deviceIO (addSensor idx par c)
            print "Sensor added."
            interpret' deviceIO next
        ControllerDef c idx next -> do
            modifyIORef deviceIO (addController idx c)
            print "Controller added."
            interpret' deviceIO next

    -}
