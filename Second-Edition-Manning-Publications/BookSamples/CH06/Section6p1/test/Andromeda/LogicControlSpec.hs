module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (createBoosters, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Andromeda.Hardware.Impl.Runtime as Impl
import qualified Andromeda.Hardware.Impl.Interpreter as Impl
import qualified Andromeda.LogicControl.Impl.Interpreter as Impl

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.LogicControl.Language as L


spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      runtime <- Impl.createHardwareRuntime

      (lStatus, rStatus) <- Impl.runLogicControl runtime aaaHardwareService $ do
        (leftBoosterCtrl, rightBoosterCtrl) <- L.evalHdl createBoosters
        lStatus <- L.evalHdl $ L.getStatus leftBoosterCtrl
        rStatus <- L.evalHdl $ L.getStatus rightBoosterCtrl
        pure (lStatus, rStatus)

      lStatus `shouldBe` StatusOk
      rStatus `shouldBe` StatusOk
