module Sandwich.Language where


import Control.Monad.Free



data BreadType
  = Baguette
  | BananaBread
  | Pumpernickel
  | Toast
  | RyeBread
  | Sourdough
  deriving (Show, Eq, Ord)

data Component
  = Bread BreadType
  | Tomato
  | Salt
  | Pepper
  | Cheese
  | Butter
  | LunchMeat
  | Lettuce
  deriving (Show, Eq, Ord)


data SandwichBody = SandwichBody BreadType [Component]
  deriving (Show, Eq, Ord)
  
data Sandwich = Sandwich BreadType (Maybe BreadType) [Component]
  deriving (Show, Eq, Ord)

data SandwichConstructor next
  = StartNewSandwich BreadType Component (SandwichBody -> next)
  | AddComponent Component SandwichBody (SandwichBody -> next)
  | FinishSandwich (Maybe BreadType) SandwichBody (Sandwich -> next)

type SandwichRecipe a = Free SandwichConstructor a







instance Functor SandwichConstructor where
  fmap f (StartNewSandwich breadType component next)
    = StartNewSandwich breadType component (f . next)
  fmap f (AddComponent component sandwichBody next)
    = AddComponent component sandwichBody (f . next)
  fmap f (FinishSandwich mbBreadType sandwichBody next)
    = FinishSandwich mbBreadType sandwichBody (f . next)


startNewSandwich :: BreadType -> Component -> SandwichRecipe SandwichBody
startNewSandwich breadType component = Free (StartNewSandwich breadType component Pure)

addComponent :: Component -> SandwichBody -> SandwichRecipe SandwichBody
addComponent component sandwichBody = Free (AddComponent component sandwichBody Pure)

finishSandwich :: Maybe BreadType -> SandwichBody -> SandwichRecipe Sandwich
finishSandwich mbBreadType sandwichBody = Free (FinishSandwich mbBreadType sandwichBody Pure)
