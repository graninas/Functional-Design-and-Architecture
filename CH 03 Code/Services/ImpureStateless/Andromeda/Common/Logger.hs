module Andromeda.Common.Logger where

data Severity = Info | Debug
  deriving (Eq, Show)
type Logger = Severity -> String -> IO ()


logMsg :: Logger
logMsg sev str = print ("["++ show sev ++ "] " ++ str)

info = Info
debug = Debug

