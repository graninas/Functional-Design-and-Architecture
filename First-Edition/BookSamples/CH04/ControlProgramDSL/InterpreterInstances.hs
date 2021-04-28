module InterpreterInstances where

import qualified ControllerDSL as C
import qualified InfrastructureDSL as I
import qualified ScriptingDSL as S
import Control as Ctrl
import Types

import Control.Monad.Free

instance C.Interpreter IO where
    onGet c p = print ("Get", c, p) >> return (StringValue "ggg")
    onSet c p v = print ("Set", c, p, v)
    onRead c si p = print ("Read", c, si, p) >> return (Measurement . FloatValue $ 33.3)
    onRun c cmd = print ("Run", c, cmd) >> return (Right "OK.")
    
instance I.Interpreter IO where
    onStoreReading r = print ("StoreReading", r)
    onSendTo r v = print ("SendTo", v) >> r v
    onGetCurrentTime = return 10

instance Ctrl.Interpreter IO where
    onEvalScript scr = S.interpretScript scr
    
    