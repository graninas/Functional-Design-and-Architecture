module IORefExample where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

second = 1000 * 1000

increaseValue :: IORef Int -> IO ()
increaseValue refVal = do
    val <- readIORef refVal
    writeIORef refVal (val + 1)
    threadDelay second

printValue :: IORef Int -> IO ()
printValue refVal = do
    val <- readIORef refVal
    print val
    threadDelay second
    
main :: IO ()
main = do
    refVal <- newIORef 0
    let worker1 = forever $ increaseValue refVal
    let worker2 = forever $ printValue refVal
    threadId1 <- forkIO worker1
    threadId2 <- forkIO worker2
    threadDelay (5 * second)
    killThread threadId1
    killThread threadId2