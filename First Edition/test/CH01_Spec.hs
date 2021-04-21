module CH01_Spec where

-- import BookSamples.CH01

import           Test.Hspec


spec :: Spec
spec =
  describe "Tests for chapter 1" $ do
    xit "Tests for chapter 1" $ do
      1 `shouldBe` 2
