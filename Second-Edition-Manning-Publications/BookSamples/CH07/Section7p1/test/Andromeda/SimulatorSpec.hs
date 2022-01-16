module Andromeda.SimulatorSpec where

import Test.Hspec

import Andromeda

import qualified Andromeda.Simulator.Runtime as SimImpl

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId)


createBoosters :: Hdl (Controller, Controller)
createBoosters = do
  lCtrl <- L.setupController lBooster lBoosterController aaaController86Passport
  L.registerComponent lCtrl nozzle1p aaaTemperature25Passport
  L.registerComponent lCtrl nozzle1t aaaPressure02Passport

  rCtrl <- L.setupController rBooster rBoosterController aaaController86Passport
  L.registerComponent rCtrl nozzle2p aaaTemperature25Passport
  L.registerComponent rCtrl nozzle2t aaaPressure02Passport
  pure (lCtrl, rCtrl)


data SimulatorControl = SimulatorControl
  { simulatorThreadId   :: ThreadId
  , simulatorRequestVar :: MVar SimulatorRequest
  }


startSimulator :: SimImpl.SimulatorRuntime -> L.LogicControl () -> IO SimulatorControl
startSimulator rt lc = do
  


reportBoostersStatus :: (Controller, Controller) -> L.LogicControl ()
reportBoostersStatus (lBoosterCtrl, rBoosterCtrl) = do
  eLStatus <- L.getStatus lBoosterCtrl
  eRStatus <- L.getStatus rBoosterCtrl
  case (eRStatus, eLStatus) of
    (Left lErr, Left rErr) -> L.report ("Hardware failure: " <> show (lErr, rErr)
    (Left lErr, _)         -> L.report ("Hardware failure: " <> show lErr)
    (_, Left rErr)         -> L.report ("Hardware failure: " <> show rErr)
    (Right ControllerOk, Right ControllerOk) -> L.report "Boosters are okay"
    err -> L.report ("Boosters are in the wrong status: " <> show err)



logicControl :: L.LogicControl ()
logicControl = do
  boostersCtrls <- L.evalHdl createBoosters
  reportBoostersStatus boostersCtrls


spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do
      simRt <- SimImpl.createSimulatorRuntime

      simControl <- startSimulator simRt logicControl

      simResult <- runSimulation leftBoosterMallfunction

      case simResult of
        Nothing -> error "Unexpected absence of results"
        Just () -> pure ()
