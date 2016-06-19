module Andromeda.Hardware.RemoteService where

import Andromeda.Hardware.HDL
import Andromeda.Hardware.Device
import Andromeda.Hardware.Runtime
import Andromeda.Hardware.Service
import Andromeda.Common.Pipe

type Command = (String, String)
type Result = String
type ServicePipe = Pipe Command Result

evaluateCommand :: Command -> String
evaluateCommand ("createDevice", args) = let
    hdl = read args :: Hdl ()
    dev = makeDevice hdl
    in (show dev)
evaluateCommand ("blankDevice", _) = show blankDevice
evaluateCommand _ = ""

serviceWorker :: ServicePipe -> IO ()
serviceWorker pipe = worker pipe evaluateCommand


createDevice' :: ServicePipe -> Hdl () -> IO Device
createDevice' pipe hdl = do
        print "Request createDevice."
        result <- sendRequest pipe ("createDevice", show hdl)
        print "Response received."
        return (read result :: Device)

blankDevice' :: ServicePipe -> IO Device
blankDevice' pipe = do
        print "Request blankDevice."
        result <- sendRequest pipe ("blankDevice", "")
        print "Response received."
        return (read result :: Device)
        
newRemoteHandle :: ServicePipe -> Handle
newRemoteHandle pipe
    = newHandle (createDevice' pipe) (blankDevice' pipe)

   


