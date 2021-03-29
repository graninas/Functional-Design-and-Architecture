module DiningPhilosophers where

import Control.Concurrent.STM
import Control.Monad (when)

data Fork = Free | InUse
  deriving Eq
data Philosopher = Thinking | Dining

takeFork :: TVar Fork -> STM ()
takeFork tvFork = do
    fork <- readTVar tvFork
    when (fork == InUse) retry
    writeTVar tvFork InUse

makeDiningPhilosopher :: TVar Fork -> TVar Fork -> STM (TVar Philosopher)
makeDiningPhilosopher leftFork rightFork = do
    takeFork leftFork
    takeFork rightFork
    newTVar Dining
    