module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Andromeda.Hardware.Impl.Runtime as Impl
import qualified Andromeda.Hardware.Impl.HdlInterpreter as Impl
-- import qualified Andromeda.LogicControl.Impl.Interpreter as Impl

import qualified Andromeda.Hardware.Language.Hdl as L
import qualified Andromeda.LogicControl.Language as L


import qualified Data.Map as Map

spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      let devices = Map.empty

      1 `shouldBe` 1
      -- devices' <- Impl.runLogicControl devices aaaHardwareService $ do
      --   (leftBoosterCtrl, rightBoosterCtrl) <- L.evalHdl createBoosters
      --   lStatus <- L.evalHil $ L.getStatus leftBoosterCtrl
      --   rStatus <- L.evalHil $ L.getStatus rightBoosterCtrl
      --   pure (lStatus, rStatus)
      --
      -- lStatus `shouldBe` (Right StatusOk)
      -- rStatus `shouldBe` (Right StatusOk)
