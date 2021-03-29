module SimulatorTest where

import Andromeda.Hardware
import Andromeda.Simulator

import SampleNetwork

import Control.Concurrent

updatingWorker :: SimulationModel -> IO ()
updatingWorker simModel1 = do
    let simModel2 = simModel1
    updatingWorker simModel2

forkUpdatingThreads :: SimulationModel -> IO ()
forkUpdatingThreads model = do
    forkIO $ updatingWorker model
    forkIO $ updatingWorker model   --  “Who I am? Why me?” - asks the thread.
    return ()

test = do
    let simModel1 = compileSimModel networkDef
    let simModel2 = updateSimulationModel simModel1
    
    print $ "initial: " ++ show (sensorsModel simModel1)
    print $ "updated: " ++ show (sensorsModel simModel2)

    forkIO $ updatingWorker simModel1
    forkIO $ updatingWorker simModel2