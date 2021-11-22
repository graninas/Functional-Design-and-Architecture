module SandwichLangSpec where

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
startNewSandwich breadType component = Free (StartNewSandwich breadType component Pure)

addComponent :: Component -> SandwichBody -> SandwichReceipt SandwichBody
addComponent component sandwichBody = Free (AddComponent component sandwichBody Pure)

finishSandwich :: Maybe BreadType -> SandwichBody -> SandwichReceipt Sandwich
finishSandwich mbBreadType sandwichBody = Free (FinishSandwich mbBreadType sandwichBody Pure)

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


-- Won't compile because it's not wrapped into Free:
-- mySandwich4 :: SandwichReceipt Sandwich
-- mySandwich4 = do
--   incomplete <- StartNewSandwich Toast Tomato ??some_continuation??
--   finishSandwich (Just Toast) incomplete





val1 :: SandwichReceipt SandwichBody
val1 = Free (StartNewSandwich Toast Tomato Pure)

val2 :: SandwichBody -> SandwichReceipt SandwichBody
val2 = \body -> Free (AddComponent Salt body Pure)

val1' :: SandwichReceipt SandwichBody
val1' = liftF (StartNewSandwich Toast Tomato id)

val2' :: SandwichBody -> SandwichReceipt SandwichBody
val2' = \body -> liftF (AddComponent Salt body id)



val = StartNewSandwich Toast Tomato
       (\b1 -> AddComponent Salt b1
          (\b2 -> AddComponent Cheese b2 (\b3 -> ()))
       )


mval1 :: SandwichBody -> SandwichConstructor ()
mval2 :: SandwichBody -> SandwichConstructor (SandwichConstructor ())
mval3 :: SandwichConstructor (SandwichConstructor (SandwichConstructor ()))
mval1 = \b -> AddComponent Cheese b (\b2 -> ())
mval2 = \b -> AddComponent Salt b mval1
mval3 = StartNewSandwich Toast Tomato mval2



spec :: Spec
spec =
  describe "Sandwich machine eDSL tests" $ do

    it "Simple sandwich scenario" $ do
      let m = mySandwich
      1 `shouldBe` 1
