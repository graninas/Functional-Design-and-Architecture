{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Control where

import ScriptingDSL
import ControllerDSL
import InfrastructureDSL
import ComputationDSL
import DataAccessDSL

import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

data Control a = forall b. EvalScript (Script b) (b -> a)

instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

type ControlProgram a = F.Free Control a

evalScript :: Script a -> ControlProgram a
evalScript scr = F.Free (EvalScript scr F.Pure)


evalInfrastructureScript scr = evalScript (InfrastructureScript scr)
evalControllerScript scr = evalScript (ControllerScript scr)
evalDataAccessScript scr = evalScript (DataAccessScript scr)
evalComputationScript scr = evalScript (ComputationScript scr)

-- example:
controlProgram :: ControlProgram ()
controlProgram = do
    logMessage info "Control program started."
    results <- startEngines
    mapM_ checkResult results
    
logMessage :: String -> String -> ControlProgram ()
logMessage severity str = do
    time <- evalInfrastructureScript getCurrentTime
    let msg = show (time, severity, str)
    evalInfrastructureScript (logMsg msg)
    
startBoosters :: ControllerScript CommandResult
startBoosters = run boostersController start

startRotaryEngines :: ControllerScript CommandResult
startRotaryEngines = run rotaryEnginesController start
    
startEngines :: ControlProgram [CommandResult]
startEngines = do
    result1 <- evalControllerScript startBoosters
    result2 <- evalControllerScript startRotaryEngines
    return [result1, result2]
    
checkResult :: CommandResult -> ControlProgram ()
checkResult (Left failed) = do
    let errorMsg = "Start engines failed"
    logMessage err errorMsg
    evalInfrastructureScript (alarm errorMsg)
checkResult (Right succeeded) = 
    logMessage info "Start engines succeeded"
    
info = "[INF]"
err = "[ERR]"
boostersController = Controller "boosters"
rotaryEnginesController = Controller "rotary engines"
start = Command "start"




    



         

         