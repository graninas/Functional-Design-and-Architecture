module Andromeda.Common.Pipe where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
    
type Request a = MVar a
type Response b = MVar b
type Pipe a b = (Request a, Response b)

createPipe :: IO (Pipe a b)
createPipe = do
    request <- newEmptyMVar
    response <- newEmptyMVar
    return (request, response)

sendRequest :: Pipe a b -> a -> IO b
sendRequest pipe@(request, response) a = do
    putMVar request a
    takeMVar response


worker :: Pipe a b -> (a -> b) -> IO ()
worker pipe@(request, response) f = forever $ do
    a <- takeMVar request
    putMVar response (f a)



