module TestProgram where

import Control.Monad.Free

import ControllerDSL as C
import InfrastructureDSL as I
import ScriptingDSL as S
import Control as Ctrl
import CustomInterpreters
import qualified InterpreterInstances as II
import Types

controlProgram :: DeviceControl ()
controlProgram = do
    logMessage "[INF]" "Control program started."
    result1 <- evalScript (controllerScript startBoosters)
    result2 <- evalScript (controllerScript startRotaryEngines)
    checkResult result1
    checkResult result2
    logMessage "[INF]" "Control program finished."
    
logMessage :: String -> String -> DeviceControl ()
logMessage severity str = do
    time <- evalScript (infrastructureScript getCurrentTime)
    let msg = show (time, severity, str)
    evalScript (infrastructureScript (logMsg msg))
    
startBoosters :: ControllerScript CommandResult
startBoosters = run (Controller "boosters") (Command "start")

startRotaryEngines :: ControllerScript CommandResult
startRotaryEngines = run (Controller "rotary engines") (Command "start")
        
checkResult :: CommandResult -> DeviceControl ()
checkResult (Left failed) = do
    let errorMsg = "Start engines failed"
    logMessage "[ERR]" errorMsg
    evalScript (infrastructureScript (alarm errorMsg))
checkResult (Right succeeded) = 
    logMessage "[INF]" "Start engines succeeded"

test1, test2 :: IO ()    
test1 = interpretDeviceControl controlProgram
test2 = Ctrl.interpret controlProgram