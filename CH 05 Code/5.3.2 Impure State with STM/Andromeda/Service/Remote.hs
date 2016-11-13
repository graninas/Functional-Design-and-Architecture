module Andromeda.Service.Remote where

import Control.Concurrent.MVar

type Request a = MVar a 
type Response b = MVar b 
type Pipe a b = (Request a, Response b)

createPipe :: IO (Pipe a b) 
createPipe = do 
    request  <- newEmptyMVar 
    response <- newEmptyMVar 
    return (request, response)

-- Sends request sync.
sendRequest :: Pipe a b -> a -> IO b 
sendRequest pipe@(request, response) a = do 
    putMVar request a 
    takeMVar response

sendResponse :: Pipe a b -> b -> IO ()
sendResponse pipe@(_, response) b = putMVar response b

getRequest :: Pipe a b -> IO a
getRequest pipe@(request, _) = takeMVar request