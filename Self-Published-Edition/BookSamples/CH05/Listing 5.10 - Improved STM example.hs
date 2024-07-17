module ImprovedStmExample where

import Control.Monad (forever, mapM_)
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.STM 

increaseValueTrans :: TVar Int -> STM Int
increaseValueTrans tvVal = do
    val <- readTVar tvVal
    writeTVar tvVal (val + 1)
    return val

increaseAndSendTrans :: TVar Int -> TMVar Int -> STM ()
increaseAndSendTrans tvVal tvSource = do 
    val <- increaseValueTrans tvVal
    putTMVar tvSource val
    
increaseValue :: TVar Int -> TMVar Int -> IO ()
increaseValue tvVal tvSource = do
    atomically $ increaseAndSendTrans tvVal tvSource
    threadDelay (1000 * 1000) -- second

receive :: TMVar Int -> IO ()
receive tvSource = do
    val <- atomically $ takeTMVar tvSource
    print val

main :: IO ()
main = do
    tvVal <- newTVarIO 0
    tvSource <- newEmptyTMVarIO
    threadId1 <- forkIO $ forever $ increaseValue tvVal tvSource
    threadId2 <- forkIO $ forever $ receive tvSource
    threadDelay (5 * 1000 * 1000) -- 5 seconds
    mapM_ killThread [threadId1, threadId2]