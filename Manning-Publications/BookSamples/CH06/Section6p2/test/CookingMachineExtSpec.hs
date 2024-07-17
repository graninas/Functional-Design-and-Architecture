{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module CookingMachineExtSpec where

import Test.Hspec

import Control.Monad.Free (Free (..), liftF)
import GHC.TypeLits


data Bread
  = Baguette
  | Toast

data Ingredient
  = Bread Bread
  | Tomato
  | Salt
  | Pepper
  | Cheese
  | Butter
  | LunchMeat
  | Lettuce

data SandwichMethod
  = StartSandwich Bread
  | AddIngredient Ingredient
  | FinishSandwich

type Temp = Symbol

data CoffeeType
  = Americano
  | Espresso

data CoffeeMethod
  = StartCoffee CoffeeType
  | AddSugar
  | KeepTemperature Temp




type Time = Symbol
type EMail = Symbol


data IAction where
  ActionWrapper :: a -> IAction

data IRecipe where
  RecipeWrapper :: a -> IRecipe

data CookingMachine
  (program :: [IAction])



type family MkAction a :: IAction where
  MkAction a = ActionWrapper a

type family MkRecipe a :: IRecipe where
  MkRecipe a = RecipeWrapper a


data SandwichRecipeImpl
  (recipe :: [SandwichMethod])
type SandwichRecipe r = MkRecipe (SandwichRecipeImpl r)

data CoffeeRecipeImpl
  (recipe :: [CoffeeMethod])
type CoffeeRecipe r = MkRecipe (CoffeeRecipeImpl r)


data ScheduleImpl
  (time :: Time)
  (recipe :: IRecipe)

data NotifyImpl
  (recipient :: EMail)

type Schedule t r = MkAction (ScheduleImpl t r)
type Notify r     = MkAction (NotifyImpl r)




type MySandwich = SandwichRecipe
  '[ StartSandwich Toast
   , AddIngredient Cheese
   , AddIngredient LunchMeat
   , FinishSandwich
   ]

type MyCoffee = CoffeeRecipe
  '[ StartCoffee Americano
   , AddSugar
   , KeepTemperature "90C"
   ]

type MyCookingProgram =
  '[ Schedule "09:00am" MySandwich
   , Schedule "09:00am" MyCoffee
   , Notify "graninas@gmail.com"
   ]



spec :: Spec
spec =
  describe "Talk tests" $ do

    xit "1" $ do
      1 `shouldBe` 1
