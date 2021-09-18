module Andromeda.LogicControl.Impl.Interpreter where

import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.Hardware.Impl.Service as Impl

runLogicControl :: Impl.HardwareService -> L.LogicControl a -> IO a
runLogicControl = error "Not implemented"
