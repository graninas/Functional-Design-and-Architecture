module SandwichLangSpec where

import Test.Hspec

import Control.Monad.Free (Free, liftF)


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

data SandwichBody = SandwichBody BreadType [Component]
data Sandwich = Sandwich BreadType [Component] (Maybe BreadType)

data SandwichConstructor next
  = StartNewSandwich BreadType Component (SandwichBody -> next)
  | AddComponent Component SandwichBody (SandwichBody -> next)
  | FinishSandwich SandwichBody (Maybe BreadType) (Sandwich -> next)

type SandwichReceipt a = Free SandwichConstructor a

instance Functor SandwichConstructor where
  fmap f (StartNewSandwich breadType component next)
    = StartNewSandwich breadType component (f . next)
  fmap f (AddComponent component sandwichBody next)
    = AddComponent component sandwichBody (f . next)
  fmap f (FinishSandwich sandwichBody mbBreadType next)
    = FinishSandwich sandwichBody mbBreadType (f . next)


startNewSandwich :: BreadType -> Component -> SandwichReceipt SandwichBody
startNewSandwich breadType component = liftF $ StartNewSandwich breadType component id

addComponent :: Component -> SandwichBody -> SandwichReceipt SandwichBody
addComponent component sandwichBody = liftF $ AddComponent component sandwichBody id

finishSandwich :: SandwichBody -> Maybe BreadType -> SandwichReceipt Sandwich
finishSandwich sandwichBody mbBreadType = liftF $ FinishSandwich sandwichBody mbBreadType id

mySandwich :: SandwichReceipt Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  finishSandwich body2 Nothing

spec :: Spec
spec =
  describe "Sandwich machine eDSL tests" $ do

    it "Simple sandwich scenario" $ do
      let m = mySandwich
      1 `shouldBe` 1
