{-# LANGUAGE GADTs #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain

import qualified Andromeda.Hardware.Language.Hdl as L

import Control.Monad.Free (Free, liftF)


data LogicControlMethod next
  = forall a. EvalHdl (L.Hdl a) (a -> next)
  | GetStatus Controller (Status -> next)

instance Functor LogicControlMethod where
  fmap f (EvalHdl hdl next) = EvalHdl hdl (f . next)
  fmap f (GetStatus controller next) = GetStatus controller (f . next)


type LogicControl a = Free LogicControlMethod a



evalHdl :: L.Hdl a -> LogicControl a
evalHdl hdl = liftF $ EvalHdl hdl id

getStatus :: Controller -> LogicControl Status
getStatus controller = liftF $ GetStatus controller id
