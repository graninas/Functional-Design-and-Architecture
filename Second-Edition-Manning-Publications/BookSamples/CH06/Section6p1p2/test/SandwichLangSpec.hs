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
data Sandwich = Sandwich BreadType (Maybe BreadType) [Component]

data SandwichConstructor next
  = StartNewSandwich BreadType Component (SandwichBody -> next)
  | AddComponent Component SandwichBody (SandwichBody -> next)
  | FinishSandwich (Maybe BreadType) SandwichBody (Sandwich -> next)

type SandwichReceipt a = Free SandwichConstructor a

instance Functor SandwichConstructor where
  fmap f (StartNewSandwich breadType component next)
    = StartNewSandwich breadType component (f . next)
  fmap f (AddComponent component sandwichBody next)
    = AddComponent component sandwichBody (f . next)
  fmap f (FinishSandwich mbBreadType sandwichBody next)
    = FinishSandwich mbBreadType sandwichBody (f . next)


startNewSandwich :: BreadType -> Component -> SandwichReceipt SandwichBody
startNewSandwich breadType component = liftF $ StartNewSandwich breadType component id

addComponent :: Component -> SandwichBody -> SandwichReceipt SandwichBody
addComponent component sandwichBody = liftF $ AddComponent component sandwichBody id

finishSandwich :: Maybe BreadType -> SandwichBody -> SandwichReceipt Sandwich
finishSandwich mbBreadType sandwichBody  = liftF $ FinishSandwich mbBreadType sandwichBody id

mySandwich :: SandwichReceipt Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  body3 <- addComponent Salt body2
  finishSandwich Nothing body3

mySandwich2 :: SandwichReceipt Sandwich
mySandwich2
    = startNewSandwich Toast Tomato
  >>= addComponent Cheese
  >>= addComponent Salt
  >>= finishSandwich Nothing

mySandwich3 :: SandwichReceipt Sandwich
mySandwich3
    = startNewSandwich Toast Tomato
    >>= (\body1 -> addComponent Cheese body1
        >>= (\body2 -> addComponent Salt body2
            >>= (\body3 -> finishSandwich (Just Toast) body3
                )
            )
        )



mySandwich4 :: SandwichReceipt Sandwich
mySandwich4 = do
  incomplete <- Free (
      StartNewSandwich Toast Tomato (\body -> Pure body)
    )
  finishSandwich (Just Toast) incomplete


-- mySandwich4 :: SandwichReceipt Sandwich
-- mySandwich4 = do
--   incomplete <- StartNewSandwich Toast Tomato ??some_continuation??
--   finishSandwich (Just Toast) incomplete


spec :: Spec
spec =
  describe "Sandwich machine eDSL tests" $ do

    it "Simple sandwich scenario" $ do
      let m = mySandwich
      1 `shouldBe` 1
