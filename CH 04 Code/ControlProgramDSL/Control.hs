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

info = "[INF]"
err = "[ERR]"
controller = Controller "boosters"
start = Command "start"

logMessage :: String -> String -> ControlProgram ()
logMessage severity str = do
    time <- evalInfrastructureScript getCurrentTime
    let msg = show (time, severity, str)
    evalInfrastructureScript (logMsg msg)

startEngines :: ControlProgram CommandResult
startEngines = evalControllerScript $ run controller start

controlProgram :: ControlProgram ()
controlProgram = do
    logMessage info "Control program started."
    result <- startEngines
    case result of
         Left failed -> do
             logMessage err "Start engines failed"
             evalInfrastructureScript (alarm "Start engines failed")
         Right succeeded -> logMessage info "Start engines succeeded"
         
         
         