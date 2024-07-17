{-# LANGUAGE GADTs #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common.Value

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as DC

import Control.Monad.Free (Free, liftF)


data LogicControlMethod next
  = forall a. EvalHdl (L.Hdl a) (a -> next)
  | forall a. EvalDeviceControlMethod (DC.DeviceControlMethod a) (a -> next)
  | Report Message (() -> next)
  | Store Key Value (() -> next)


instance Functor LogicControlMethod where
  fmap f (EvalHdl hdl next) = EvalHdl hdl (f . next)
  fmap f (EvalDeviceControlMethod dc next) = EvalDeviceControlMethod dc (f . next)
  fmap f (Report msg next) = Report msg (f . next)
  fmap f (Store key value next) = Store key value (f . next)


type LogicControl a = Free LogicControlMethod a


evalHdl :: L.Hdl a -> LogicControl a
evalHdl hdl = liftF $ EvalHdl hdl id

evalDeviceControl :: DC.DeviceControlMethod a -> LogicControl a
evalDeviceControl dc = liftF $ EvalDeviceControlMethod dc id

report :: Message -> LogicControl ()
report msg = liftF $ Report msg id

store :: Key -> Value -> LogicControl ()
store key value = liftF $ Store key value id






getStatus :: Controller -> LogicControl (Either HardwareFailure ControllerStatus)
getStatus ctrl = evalDeviceControl $ DC.getStatus' ctrl

readSensor :: Controller -> ComponentIndex -> LogicControl (Either HardwareFailure Measurement)
readSensor ctrl idx = evalDeviceControl $ DC.readSensor' ctrl idx
