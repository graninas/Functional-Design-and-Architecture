module SandwichLangSpec where

import Test.Hspec

import Control.Monad.Free (Free (..), liftF)

import Sandwich.Language
import Sandwich.ImpureInterpreter



mySandwich :: SandwichRecipe Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  body3 <- addComponent Salt body2
  finishSandwich Nothing body3

mySandwich2 :: SandwichRecipe Sandwich
mySandwich2
    = startNewSandwich Toast Tomato
  >>= addComponent Cheese
  >>= addComponent Salt
  >>= finishSandwich Nothing

mySandwich3 :: SandwichRecipe Sandwich
mySandwich3
    = startNewSandwich Toast Tomato
    >>= (\body1 -> addComponent Cheese body1
        >>= (\body2 -> addComponent Salt body2
            >>= (\body3 -> finishSandwich (Just Toast) body3
                )
            )
        )



mySandwich4 :: SandwichRecipe Sandwich
mySandwich4 = do
  incomplete <- Free (
      StartNewSandwich Toast Tomato (\body -> Pure body)
    )
  finishSandwich (Just Toast) incomplete


-- Won't compile because it's not wrapped into Free:
-- mySandwich4 :: SandwichRecipe Sandwich
-- mySandwich4 = do
--   incomplete <- StartNewSandwich Toast Tomato ??some_continuation??
--   finishSandwich (Just Toast) incomplete





val1 :: SandwichRecipe SandwichBody
val1 = Free (StartNewSandwich Toast Tomato Pure)

val2 :: SandwichBody -> SandwichRecipe SandwichBody
val2 = \body -> Free (AddComponent Salt body Pure)

val1' :: SandwichRecipe SandwichBody
val1' = liftF (StartNewSandwich Toast Tomato id)

val2' :: SandwichBody -> SandwichRecipe SandwichBody
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
