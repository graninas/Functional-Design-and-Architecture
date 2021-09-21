-- {-# LANGUAGE GADTs #-}

module Andromeda.LogicControl.Language where

import Andromeda.Hardware.Domain
import Andromeda.LogicControl.Domain
import Andromeda.Common.Value

import qualified Andromeda.Hardware.Language.Hdl as L

data LogicControlMethod next
  = EvalHdl L.Hdl
  -- | Report Message
  -- | Store Key Value


type LogicControl = [LogicControlMethod]
