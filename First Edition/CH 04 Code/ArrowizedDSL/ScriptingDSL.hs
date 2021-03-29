module ScriptingDSL where

import ControllerDSL
import InfrastructureDSL
import ComputationDSL
import DataAccessDSL

import Control.Monad.Free

data Script b = ControllerScript (ControllerScript b)
              | ComputationScript (ComputationScript b)
              | InfrastructureScript (InfrastructureScript b)
              | DataAccessScript (DataAccessScript b)
              
infrastructureScript :: InfrastructureScript b -> Script b
infrastructureScript = InfrastructureScript

controllerScript :: ControllerScript b -> Script b
controllerScript = ControllerScript



