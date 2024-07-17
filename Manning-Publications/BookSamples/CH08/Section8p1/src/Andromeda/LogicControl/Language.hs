{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as DC

import Control.Monad.Free (Free, liftF)


data LogicControlMethod next
  = forall a. EvalHdl (L.Hdl a) (a -> next)
  | forall a. EvalDeviceControlMethod (DC.DeviceControlMethod a) (a -> next)
  | Report Message (() -> next)
  | Store Key Value (() -> next)
  | Load Key (Maybe Value -> next)


instance Functor LogicControlMethod where
  fmap f (EvalHdl hdl next) = EvalHdl hdl (f . next)
  fmap f (EvalDeviceControlMethod dc next) = EvalDeviceControlMethod dc (f . next)
  fmap f (Report msg next) = Report msg (f . next)
  fmap f (Store key value next) = Store key value (f . next)
  fmap f (Load key next) = Load key (f . next)


newtype LogicControl a = LogicControl (Free LogicControlMethod a)
  deriving (Functor, Applicative, Monad)


evalHdl :: L.Hdl a -> LogicControl a
evalHdl hdl = LogicControl $ liftF $ EvalHdl hdl id

evalDeviceControl :: DC.DeviceControlMethod a -> LogicControl a
evalDeviceControl dc = LogicControl $ liftF $ EvalDeviceControlMethod dc id

report :: Message -> LogicControl ()
report msg = LogicControl $ liftF $ Report msg id

store :: Key -> Value -> LogicControl ()
store key value = LogicControl $ liftF $ Store key value id

load :: Key -> LogicControl (Maybe Value)
load key = LogicControl $ liftF $ Load key id




wrapHardwareFailure :: Either HardwareFailure a -> Either LogicControlFailure a
wrapHardwareFailure (Left err) = Left (HardwareFailure err)
wrapHardwareFailure (Right v) = Right v

getStatus :: Controller -> LogicControl (Either LogicControlFailure ControllerStatus)
getStatus ctrl
  = wrapHardwareFailure
  <$> (evalDeviceControl $ DC.getStatus' ctrl)

readSensor :: Controller -> ComponentIndex -> LogicControl (Either LogicControlFailure SensorMeasurement)
readSensor ctrl idx
  = wrapHardwareFailure
  <$> (evalDeviceControl $ DC.readSensor' ctrl idx)

getProperty :: Controller -> PropertyName -> [Param] -> LogicControl (Either LogicControlFailure (Maybe Property))
getProperty ctrl propName params
  = wrapHardwareFailure
  <$> (evalDeviceControl $ DC.getProperty' ctrl propName params)

evalCommand :: Controller -> Command -> [Param] -> LogicControl (Either LogicControlFailure CommandResult)
evalCommand ctrl cmd params
  = wrapHardwareFailure
  <$> (evalDeviceControl $ DC.evalCommand' ctrl cmd params)
