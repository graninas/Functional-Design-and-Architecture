module Andromeda.Hardware.Impl.Interpreter where

import qualified Andromeda.Hardware.Language.Hdl as L

import qualified Andromeda.Hardware.Impl.Service as Impl

runHdl :: Impl.HardwareService -> L.Hdl a -> IO a
runHdl = error "Not implemented"
