module Sandwich2LangSpec where

import Test.Hspec

import Control.Monad.Free (Free (..), liftF)


data BreadType
  = Baguette
  | BananaBread
  | Pumpernickel
  | Toast
  | RyeBread
  | Sourdough

data Component
  = Tomato
  | Salt
  | Pepper
  | Cheese
  | Butter
  | LunchMeat
  | Lettuce


data Sandwich = Sandwich
  BreadType                 -- base
  (Maybe BreadType)         -- possibly a top
  (List Component)          -- ingredients

data SandwichBody = SandwichBody BreadType (List Component)


data Sandwich2 = Sandwich2
  { base       :: BreadType
  , top        :: Maybe BreadType
  , components :: [Component]
  }



data List a
  = Cons a (List a)
  | Nil


mySandwich :: Sandwich
mySandwich = Sandwich
  Toast
  (Just Toast)
  (Cons Salt (Cons Cheese Nil))


mySandwich2 :: Sandwich2
mySandwich2 = Sandwich2
  Toast
  (Just Toast)
  [Salt, Cheese]


spec :: Spec
spec =
  describe "Sandwich machine eDSL tests" $ do

    it "Simple sandwich scenario" $ do
      let m = mySandwich
      1 `shouldBe` 1
