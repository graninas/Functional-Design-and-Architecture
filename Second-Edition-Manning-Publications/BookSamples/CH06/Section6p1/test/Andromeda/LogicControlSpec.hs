module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)


spec :: Spec
spec =
  describe "Logic Control tests" $ do

    it "Controller status check" $ do

      status <- runLogicControl aaaHardwareService $ do
        boostersCtrl <- initDevice boostersDef
        getStatus boostersCtrl

      status `shouldBe` StatusOk
