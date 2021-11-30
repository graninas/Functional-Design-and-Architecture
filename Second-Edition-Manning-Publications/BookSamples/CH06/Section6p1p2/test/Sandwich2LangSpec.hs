module Sandwich2LangSpec where

import Test.Hspec

import Control.Monad.Free (Free (..), liftF)
import Data.Map (Map)
import qualified Data.Map as Map


data BreadType
  = Baguette
  | BananaBread
  | Pumpernickel
  | Toast
  | RyeBread
  | Sourdough
  deriving (Show, Eq, Ord)


data Component
  = Tomato
  | Salt
  | Pepper
  | Cheese
  | Butter
  | LunchMeat
  | Lettuce
  deriving (Show, Eq, Ord)

data Sandwich = Sandwich
  { base       :: BreadType
  , top        :: Maybe BreadType
  , components :: [Component]
  }
  deriving (Show, Eq, Ord)

mySandwich :: Sandwich
mySandwich = Sandwich
  Toast
  (Just Toast)
  [Salt, Cheese]


data SandwichConstructor
  = StartNewSandwich BreadType Component
  | AddComponent Component
  | FinishSandwich (Maybe BreadType)


type Receipe = [SandwichConstructor]

mySandwichReceipe :: Receipe
mySandwichReceipe =
  [ StartNewSandwich Toast Cheese
  , AddComponent Salt
  , FinishSandwich (Just Toast)
  ]


type Ingredient = Either BreadType Component
type Ingredients = Map Ingredient Int

data CookingProblem
  = InvalidReceipe
  | LackOfIngredients String
  deriving (Show, Eq, Ord)


doCreateSandwich :: Receipe -> Ingredients -> Sandwich -> Either CookingProblem Sandwich
doCreateSandwich [] _ s = Right s
doCreateSandwich (StartNewSandwich _ _ : _) _ _ = Left InvalidReceipe

doCreateSandwich (AddComponent component : rs) ingredients oldS = let
  mbComponentPieces = Map.lookup (Right component) ingredients
  in case mbComponentPieces of
    Nothing -> Left $ LackOfIngredients $ show component
    Just cCnt -> let
      ingredients' = Map.insert (Right component) (cCnt - 1) ingredients
      Sandwich baseBread mbTop components = oldS
      newS = Sandwich baseBread mbTop (component : components)
      in doCreateSandwich rs ingredients' newS

doCreateSandwich (FinishSandwich Nothing : []) ingredients oldS = Right oldS

doCreateSandwich (FinishSandwich (Just topBread) : []) ingredients oldS = let
  mbBreadPieces = Map.lookup (Left topBread) ingredients
  in case mbBreadPieces of
    Nothing -> Left $ LackOfIngredients $ show topBread
    Just bCnt -> let
      Sandwich baseBread _ components = oldS
      in Right $ Sandwich baseBread (Just topBread) components

doCreateSandwich (FinishSandwich _ : _) _ _ = Left InvalidReceipe




createSandwich :: Receipe -> Ingredients -> Either CookingProblem Sandwich
createSandwich [] _ = Left InvalidReceipe

createSandwich (StartNewSandwich baseBread component : rs) ingredients = let
  mbBreadPieces = Map.lookup (Left baseBread) ingredients
  mbComponentPieces = Map.lookup (Right component) ingredients
  in case (mbBreadPieces, mbComponentPieces) of
    (Nothing, _) -> Left $ LackOfIngredients $ show baseBread
    (_, Nothing) -> Left $ LackOfIngredients $ show component
    (Just bCnt, Just cCnt)  -> let
      ingredients'  = Map.insert (Left baseBread) (bCnt - 1) ingredients
      ingredients'' = Map.insert (Right component) (cCnt - 1) ingredients'
      base = Sandwich baseBread Nothing [component]
      in doCreateSandwich rs ingredients'' base

createSandwich _ _ = Left InvalidReceipe




-- we can easily create an invalid receipe:

-- starts not from StartNewSandwich procedure
-- mySandwichReceipe =
--   [ AddComponent Salt
--   , StartNewSandwich Toast Cheese
--   , FinishSandwich (Just Toast)
--   ]

-- to solve that, we need to make the steps depending on each other.
-- Continuations + free monads + intermediate data types


spec :: Spec
spec =
  describe "Sandwich machine eDSL tests" $ do

    it "Simple sandwich scenario" $ do
      let m = mySandwich
      1 `shouldBe` 1
