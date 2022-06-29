module Andromeda.SimulatorSpec where

import Test.Hspec

import Andromeda

import qualified Andromeda.LogicControl.Domain as T
import qualified Andromeda.LogicControl.Language as L
import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L

import qualified Andromeda.Simulator.Runtime as SimImpl
import qualified Andromeda.Simulator.Control as SimImpl

import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent (ThreadId)



getBoostersStatus
  :: (Controller, Controller)
  -> L.LogicControl (Either String ControllerStatus)
getBoostersStatus (lBoosterCtrl, rBoosterCtrl) = do
  eLStatus <- L.getStatus lBoosterCtrl
  eRStatus <- L.getStatus rBoosterCtrl

  case (eRStatus, eLStatus) of
    (Right ControllerOk, Right ControllerOk) -> pure $ Right ControllerOk
    (Left lErr, Left rErr) -> pure $ Left $ "Hardware failure: " <> show (lErr, rErr)
    (Left lErr, _)         -> pure $ Left $ "Left booster failure: " <> show lErr
    (_, Left rErr)         -> pure $ Left $ "Right booster failure: " <> show rErr
    err -> pure $ Left $ "Boosters are in the wrong status: " <> show err

reportBoostersStatus :: L.LogicControl ()
reportBoostersStatus = do
  boostersCtrls <- L.evalHdl createBoosters
  eStatus <- getBoostersStatus boostersCtrls
  case eStatus of
    Right ControllerOk -> L.report "Boosters are okay"
    Left err           -> L.report err


readBoostersTemperature
  :: (Controller, Controller)
  -> L.LogicControl (Either String (Float, Float))
readBoostersTemperature (lBoosterCtrl, rBoosterCtrl) = do
  eLTemperature <- L.readSensor lBoosterCtrl nozzle1t
  eRTemperature <- L.readSensor rBoosterCtrl nozzle2t

  case (eLTemperature, eRTemperature) of
    ( Right (SensorMeasurement (UnitTemperature (Kelvin lVal))),
      Right (SensorMeasurement (UnitTemperature (Kelvin rVal))))
        -> pure $ Right (lVal, rVal)
    (Left lErr, Left rErr) -> pure $ Left $ "Hardware failure: " <> show (lErr, rErr)
    (Left lErr, _)         -> pure $ Left $ "Left booster failure: " <> show lErr
    (_, Left rErr)         -> pure $ Left $ "Right booster failure: " <> show rErr
    err -> pure $ Left $ "Wrong boosters response: " <> show err

reportBoostersTemperature :: L.LogicControl ()
reportBoostersTemperature = do
  boostersCtrls <- L.evalHdl createBoosters
  eTemperature <- readBoostersTemperature boostersCtrls
  case eTemperature of
    Left err       -> L.report err
    Right (v1, v2) -> do
      L.report $ "T1 = " <> show v1 <> ", T2 = " <> show v2
      L.report "Temperature values are okay."


spec :: Spec
spec =
  describe "Simulator tests" $ do

    it "Boosters status check" $ do
      simRt <- SimImpl.createSimulatorRuntime
      simControl <- SimImpl.startSimulator simRt

      SimImpl.runSimulation simControl reportBoostersStatus

      simControl <- SimImpl.stopSimulator simRt simControl

      let SimImpl.SimulatorRuntime{simRtMessagesVar, simRtErrorsVar} = simRt
      mbMsgs <- tryReadMVar simRtMessagesVar
      mbErrs <- tryReadMVar simRtErrorsVar
      case (mbMsgs, mbErrs) of
        (Just msgs, Just []) -> msgs `shouldBe` ["Boosters are okay"]
        results -> error $ "Unexpected results: " <> show results

    it "Boosters temperature check" $ do
      simRt <- SimImpl.createSimulatorRuntime
      simControl <- SimImpl.startSimulator simRt

      SimImpl.runSimulation simControl reportBoostersTemperature

      simControl <- SimImpl.stopSimulator simRt simControl

      let SimImpl.SimulatorRuntime{simRtMessagesVar, simRtErrorsVar} = simRt
      mbMsgs <- tryReadMVar simRtMessagesVar
      mbErrs <- tryReadMVar simRtErrorsVar
      case (mbMsgs, mbErrs) of
        (Just [msg,_], Just []) -> msg `shouldBe` "Temperature values are okay."
        results -> error $ "Unexpected results: " <> show results
