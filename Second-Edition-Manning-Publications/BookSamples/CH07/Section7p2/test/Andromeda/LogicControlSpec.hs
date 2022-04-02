module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Andromeda.Hardware.Impl.Device.Types as TImpl
import qualified Andromeda.Hardware.Impl.Service as SImpl
import qualified Andromeda.Hardware.Impl.Runtime as RImpl
import qualified Andromeda.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Andromeda.Hardware.Impl.Interpreters.DeviceControl as DCImpl

import qualified Andromeda.LogicControl.Impl.Interpreter as LCImpl

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L

import qualified Andromeda.TestData.Scripts as Test


getBoostersStatus :: LogicControl (Either LogicControlFailure (ControllerStatus, ControllerStatus))
getBoostersStatus = do
  (lCtrl, rCtrl) <- L.evalHdl Test.createBoosters
  eLStatus <- Test.getControllerStatus lCtrl
  eRStatus <- Test.getControllerStatus rCtrl
  pure $ case (eLStatus, eRStatus) of
    (Right s1, Right s2) -> Right (s1, s2)
    (Left e, _) -> Left $ LogicControlFailure $ show e
    (_, Left e) -> Left $ LogicControlFailure $ show e


spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      runtime <- RImpl.createHardwareRuntime aaaHardwareService

      eResult <- LCImpl.runLogicControl runtime getBoostersStatus

      case eResult of
        Left e -> fail $ show e
        Right (lStatus, rStatus) -> do
          lStatus `shouldBe` ControllerOk
          rStatus `shouldBe` ControllerOk
