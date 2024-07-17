module ScriptingDSL where

import qualified ControllerDSL as C
import qualified InfrastructureDSL as I
import qualified ComputationDSL as Comp
import qualified DataAccessDSL as DA

import Control.Monad.Free

data Script b = ControllerScriptDef (C.ControllerScript b)
              | ComputationScriptDef (Comp.ComputationScript b)
              | InfrastructureScriptDef (I.InfrastructureScript b)
              | DataAccessScriptDef (DA.DataAccessScript b)
              
controllerScript :: C.ControllerScript b -> Script b
controllerScript = ControllerScriptDef

infrastructureScript :: I.InfrastructureScript b -> Script b
infrastructureScript =InfrastructureScriptDef

computationScript :: Comp.ComputationScript b -> Script b
computationScript = ComputationScriptDef

dataAccessScript :: DA.DataAccessScript b -> Script b
dataAccessScript = DataAccessScriptDef

interpretScript (ControllerScriptDef scr)     = C.interpret scr
interpretScript (InfrastructureScriptDef scr) = I.interpret scr
--interpretScript (ComputationScriptDef scr) = return () -- TODO
--interpretScript (DataAccessScriptDef scr) = return () -- TODO