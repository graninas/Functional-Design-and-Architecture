module StmExample where

import Control.Monad (forever, mapM_)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.STM 
    (newTVarIO, readTVar, writeTVar, atomically, TVar, STM (..))

increaseValueTrans :: TVar Int -> STM ()
increaseValueTrans tvVal = do
    val <- readTVar tvVal
    writeTVar tvVal (val + 1)

increaseValue :: TVar Int -> IO ()
increaseValue tvVal = do
    atomically $ increaseValueTrans tvVal
    threadDelay (1000 * 1000) -- second

printValue :: TVar Int -> IO ()
printValue tvVal = do
    val <- atomically $ readTVar tvVal
    print val
    threadDelay (1000 * 1000) -- second
    
main :: IO ()
main = do
    tvVal <- newTVarIO 0
    threadId1 <- forkIO $ forever $ increaseValue tvVal
    threadId2 <- forkIO $ forever $ printValue tvVal
    threadDelay (5 * 1000 * 1000) -- 5 seconds
    mapM_ killThread [threadId1, threadId2]