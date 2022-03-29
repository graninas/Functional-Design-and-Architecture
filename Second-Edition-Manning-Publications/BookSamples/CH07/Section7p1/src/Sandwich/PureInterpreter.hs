module Sandwich.PureInterpreter where

import Sandwich.Language

import Control.Monad.Free
import qualified Data.Map as Map


type Ingredients = Map.Map Component Int
type CookingProblem = String

withdrawIngredient
  :: Ingredients
  -> Component
  -> Either CookingProblem Ingredients
withdrawIngredient ingreds component =
  case Map.lookup component ingreds of
    Just count | count > 0 -> let
      ingreds' = Map.insert component (count - 1) ingreds
      in Right ingreds'
    _ -> Left ("No ingredient: " <> show component)

interpretStep
  :: Ingredients
  -> SandwichConstructor a
  -> Either CookingProblem (Ingredients, a)
interpretStep ingreds (StartNewSandwich bread component next) = do
  ingreds1 <- withdrawIngredient ingreds component
  ingreds2 <- withdrawIngredient ingreds1 (Bread bread)
  let body = SandwichBody bread [component]
  pure (ingreds2, next body)

interpretStep ingreds (AddComponent component body next) = do
  ingreds1 <- withdrawIngredient ingreds component
  let SandwichBody bread components = body
  let body' = SandwichBody bread (component : components)
  pure (ingreds1, next body')

interpretStep ingreds (FinishSandwich mbBread body next) = do
  let SandwichBody bread components = body
  ingreds' <- case mbBread of
    Nothing     -> Right ingreds
    Just bread' -> withdrawIngredient ingreds (Bread bread')
  pure (ingreds', next (Sandwich bread mbBread components))



interpretRecipe
  :: Ingredients
  -> SandwichRecipe Sandwich
  -> Either CookingProblem (Ingredients, Sandwich)
interpretRecipe ingreds (Pure sandwich) = Right (ingreds, sandwich)
interpretRecipe ingreds (Free step) =
  case interpretStep ingreds step of
    Left err -> Left err
    Right (ingreds', next) -> interpretRecipe ingreds' next


makeSandwich
  :: Ingredients
  -> SandwichRecipe Sandwich
  -> Either CookingProblem (Ingredients, Sandwich)
makeSandwich ingreds recipe = interpretRecipe ingreds recipe
