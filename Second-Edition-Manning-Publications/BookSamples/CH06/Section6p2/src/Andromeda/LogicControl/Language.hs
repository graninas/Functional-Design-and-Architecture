{-# LANGUAGE GADTs #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common.Value

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L

import Control.Monad.Free (Free, liftF)


data LogicControlMethod next
  = forall a. EvalHdl (L.Hdl a) (a -> next)
  | forall a. EvalDeviceControl (L.DeviceControl a) (a -> next)
  | Report Message (() -> next)
  | Store Key Value (() -> next)


instance Functor LogicControlMethod where
  fmap f (EvalHdl hdl next) = EvalHdl hdl (f . next)
  fmap f (EvalDeviceControl hdl next) = EvalDeviceControl hdl (f . next)
  fmap f (Report msg next) = Report msg (f . next)
  fmap f (Store key value next) = Store key value (f . next)


type LogicControl a = Free LogicControlMethod a


evalHdl :: L.Hdl a -> LogicControl a
evalHdl hdl = liftF $ EvalHdl hdl id

evalDeviceControl :: L.DeviceControl a -> LogicControl a
evalDeviceControl hdl = liftF $ EvalDeviceControl hdl id

report :: Message -> LogicControl ()
report msg = liftF $ Report msg id

store :: Key -> Value -> LogicControl ()
store key value = liftF $ Store key value id
