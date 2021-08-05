
module Andromeda.HardwareSpec where

import           Test.Hspec
import           Data.Proxy (Proxy(..))

spec :: Spec
spec =
  describe "Hardware tests" $ do
    it "dummy" $ do
      1 `shouldBe` 1
