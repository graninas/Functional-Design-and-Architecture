module Sandwich.StatefulInterpreter where

import Sandwich.Language

import Control.Monad.Free
import qualified Data.Map as Map
import Data.IORef


type Ingredients = Map.Map Component Int

withdrawIngredient :: IORef Ingredients -> Component -> IO ()
withdrawIngredient ingredsRef component = do
  ingreds <- readIORef ingredsRef
  case Map.lookup component ingreds of
    Nothing -> error ("Ingredient not found: " <> show component)
    Just count | count <= 0 -> error ("No ingredient: " <> show component)
               | otherwise  -> do
      let ingreds' = Map.insert component (count - 1) ingreds
      writeIORef ingredsRef ingreds'

interpretStep :: IORef Ingredients -> SandwichConstructor a -> IO a
interpretStep ingredsRef (StartNewSandwich bread component next) = do
  withdrawIngredient ingredsRef component
  withdrawIngredient ingredsRef (Bread bread)
  let body = SandwichBody bread [component]
  pure (next body)

interpretStep ingredsRef (AddComponent component body next) = do
  withdrawIngredient ingredsRef component
  let SandwichBody bread components = body
  let body' = SandwichBody bread (component : components)
  pure (next body')

interpretStep ingredsRef (FinishSandwich mbBread body next) = do
  let SandwichBody bread components = body
  case mbBread of
        Nothing     -> pure ()
        Just bread' -> withdrawIngredient ingredsRef (Bread bread')
  pure (next (Sandwich bread mbBread components))



interpretRecipe :: IORef Ingredients -> SandwichRecipe Sandwich -> IO Sandwich
interpretRecipe ingredsRef (Pure sandwich) = pure sandwich
interpretRecipe ingredsRef (Free step) = do
  next <- interpretStep ingredsRef step
  interpretRecipe ingredsRef next


createSandwich :: IORef Ingredients -> SandwichRecipe Sandwich -> IO Sandwich
createSandwich ingredsRef recipe = interpretRecipe ingredsRef recipe
