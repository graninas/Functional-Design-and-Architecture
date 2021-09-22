module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (createBoosters, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Andromeda.Hardware.Impl.Runtime as Impl
import qualified Andromeda.Hardware.Impl.HdlInterpreter as Impl
import qualified Andromeda.Hardware.Impl.DeviceControlInterpreter as Impl
import qualified Andromeda.LogicControl.Impl.Interpreter as Impl

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.Hardware.Language.DeviceControl as L
import qualified Andromeda.LogicControl.Language as L


spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      runtime <- Impl.createHardwareRuntime

      (lStatus, rStatus) <- Impl.runLogicControl runtime aaaHardwareService $ do
        (leftBoosterCtrl, rightBoosterCtrl) <- L.evalHdl createBoosters
        lStatus <- L.evalDeviceControl $ L.getStatus leftBoosterCtrl
        rStatus <- L.evalDeviceControl $ L.getStatus rightBoosterCtrl
        pure (lStatus, rStatus)

      lStatus `shouldBe` (Right StatusOk)
      rStatus `shouldBe` (Right StatusOk)
