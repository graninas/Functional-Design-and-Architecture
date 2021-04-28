module Interpreters where

import ControllerDSL as C
import InfrastructureDSL as I
import ScriptingDSL as S
import Control

import Control.Monad.Free


interpretScript (ControllerScript scr)     = interpretControllerScript scr
interpretScript (InfrastructureScript scr) = interpretInfrastructureScript scr

interpretInfrastructureScript (Pure a) = return a
interpretInfrastructureScript (Free (StoreReading r next)) = do
    print ("StoreReading", r)
    interpretInfrastructureScript next
interpretInfrastructureScript (Free (SendTo r v next)) = do
    print ("SendTo", v)
    r v
    interpretInfrastructureScript next
interpretInfrastructureScript (Free (GetCurrentTime next)) = do
    print "GetCurrentTime"
    interpretInfrastructureScript (next 10)


interpretControllerScript (Pure a) = return a
interpretControllerScript (Free (Get c p next)) = do
    print ("Get", c, p)
    interpretControllerScript (next (StringValue "ggg"))
interpretControllerScript (Free (Set c p v next)) = do
    print ("Get", c, p, v)
    interpretControllerScript next
interpretControllerScript (Free (Read c si p next)) = do
    print ("Read", c, si, p)
    interpretControllerScript (next (Measurement . FloatValue $ 33.3))
interpretControllerScript (Free (Run c cmd next)) = do
    print ("Run", c, cmd)
    interpretControllerScript (next (Right "OK."))
    
    
interpretControlProgram :: ControlProgram a -> IO a
interpretControlProgram (Pure a) = return a
interpretControlProgram (Free (EvalScript scr next)) = do
    res <- interpretScript scr
    interpretControlProgram (next res)