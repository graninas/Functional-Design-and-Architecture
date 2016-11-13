module DiningPhilosophers where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when, join, mapM)
import Data.List (permutations)


data Usage = Free | InUse  deriving (Eq, Show)
data Fork = Fork Usage Int deriving (Eq, Show)
data Action = Thinking | Dining deriving (Eq, Show)
data Philosopher = Philosopher Action deriving (Eq, Show)

takeFork :: TVar Fork -> STM ()
takeFork fork = do
    Fork usage n <- readTVar fork
    when (usage == InUse) retry
    writeTVar fork $ Fork InUse n

makeDiningPhilosopher :: TVar Fork -> TVar Fork -> STM (TVar Philosopher)
makeDiningPhilosopher leftFork rightFork = do
    takeFork leftFork
    takeFork rightFork
    newTVar $ Philosopher Dining

philosophers = join $ permutations [1..5]

printForkPair (left, right) = do
    [l, r] <- mapM (atomically . readTVar) [left, right]
    print (l, r)

philosopherCycle :: IO ()
philosopherCycle = do
    
    
main = do
    forks@[f1, f2, f3, f4, f5] <- mapM (newTVarIO . Fork Free) [1..5]
    let forkPairs = zip forks (tail $ cycle $ forks)
    
    mapM_ printForkPair forkPairs
    
    <- 
    
    --philosopher <- atomically $ makeDiningPhilosopher
    