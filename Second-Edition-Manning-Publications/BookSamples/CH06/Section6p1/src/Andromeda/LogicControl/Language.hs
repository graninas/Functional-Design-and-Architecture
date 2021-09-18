module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain



data LogicControlMethod next
  = forall a. EvalHdl (Hdl a) (a -> next)
  | GetStatus Controller (Status -> next)

instance Functor LogicControlMethod where
  fmap f (EvalHdl hdl next) = EvalHdl hdl (f . next)
  fmap f (GetStatus controller next) = GetStatus controller (f . next)


type LogicControl a = Free LogicControlMethod a



evalHdl :: Hdl a -> LogicControl a
evalHdl hdl = liftF $ EvalHdl hdl id

getStatus :: Controller -> LogicControl Status
getStatus controller = liftF $ GetStatus controller id

initDevice :: Hdl Controller -> LogicControl Controller
initDevice = evalHdl
