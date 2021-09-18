module Andromeda.LogicControlSpec where

import Test.Hspec

import Andromeda

import Andromeda.Assets (boostersDef, aaaController86Name)
import Andromeda.Assets.Vendors.AAA.HardwareService (aaaHardwareService)



spec :: Spec
spec =
  describe "Logic control tests" $ do

    it "Controller properties" $ do
      1 `shouldBe` 1
