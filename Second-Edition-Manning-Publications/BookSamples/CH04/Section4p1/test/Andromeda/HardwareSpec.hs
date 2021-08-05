
module Andromeda.HardwareSpec where

import           Test.Hspec

import Andromeda

import Andromeda.Assets.Hardware.Components (boostersDef)
import Andromeda.Vendors.AAA (c86ControllerName)

spec :: Spec
spec =
  describe "Hardware tests" $ do
    it "Hardware device components check" $ do

      let boosters = makeDevice boostersDef
      let mbThermometer = getComponent "controller" boosters

      case mbThermometer of
        Nothing -> fail "There is no such component"
        Just component -> putStrLn "Component found."
