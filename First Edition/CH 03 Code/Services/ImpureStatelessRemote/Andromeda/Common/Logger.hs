module Andromeda.Common.Logger where

import Control.Concurrent.MVar

-- TODO: this is not working.

data Severity = Info | Debug
  deriving (Eq, Show)
data Logger = Logger {
    logLock :: MVar String }

type LogMsgAction = Logger -> Severity -> String -> IO ()

logMsg :: LogMsgAction
logMsg (Logger lock) sev str = do
    putMVar lock ""
    
    takeMVar lock
    print ("["++ show sev ++ "] " ++ str)


info = Info
debug = Debug



