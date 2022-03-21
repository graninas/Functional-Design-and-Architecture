module Sandwich.PureInterpreter where

import Sandwich.Language

import Control.Monad.Free
import qualified Data.Map as Map
import Data.IORef


type Ingredients = Map.Map Component Int
type CookingProblem = String

withdrawIngredient
  :: Ingredients
  -> Component
  -> Either CookingProblem Ingredients
withdrawIngredient ingreds component =
  case Map.lookup component ingreds of
    Nothing -> Left ("No ingredient: " <> show component)
    Just count | count <= 0 -> Left ("No ingredient: " <> show component)
               | otherwise  -> let
      ingreds' = Map.insert component (count - 1) ingreds
      in Right ingreds'

interpretStep
  :: Ingredients
  -> SandwichConstructor a
  -> Either CookingProblem (Ingredients, a)
interpretStep = undefined


interpretRecipe
  :: Ingredients
  -> SandwichRecipe Sandwich
  -> Either CookingProblem (Ingredients, Sandwich)
interpretRecipe ingreds (Pure sandwich) = Right (ingreds, sandwich)
interpretRecipe ingreds (Free step) = eResult
  where
    eStepResult = interpretStep ingreds step
    eResult = case eStepResult of
      Left err -> Left err
      Right (ingreds', next) -> interpretRecipe ingreds' next


makeSandwich
  :: Ingredients
  -> SandwichRecipe Sandwich
  -> Either CookingProblem (Ingredients, Sandwich)
makeSandwich ingreds recipe = interpretRecipe ingreds recipe
