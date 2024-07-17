-- {-# LANGUAGE GADTs #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common.Value

import qualified Andromeda.Hardware.Language.Hdl as L

data LogicControlMethod
  = EvalHdl L.Hdl

type LogicControl = [LogicControlMethod]
