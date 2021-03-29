module STMBlocking where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad


transaction :: TVar Bool -> STM Int
transaction tvFlag = do
    flag <- readTVar tvFlag
    if flag
        then return 100
        else retry
        
worker tvFlag = do
    print "Trying..."
    val <- atomically $ transaction tvFlag
    print val
        
test = do
    tvFlag <- newTVarIO False
    worker tvFlag
