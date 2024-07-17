module Sandwich.ImpureInterpreter where

import Sandwich.Language

import Control.Monad.Free
import qualified Data.Map as Map
import Data.IORef


type Ingredients = Map.Map Component Int

withdrawIngredient :: IORef Ingredients -> Component -> IO ()
withdrawIngredient ingredsRef component = do
  ingreds <- readIORef ingredsRef
  case Map.lookup component ingreds of
    Just count | count > 0 -> do
      let ingreds' = Map.insert component (count - 1) ingreds
      writeIORef ingredsRef ingreds'
    _ -> error ("No ingredient: " <> show component)

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


makeSandwich :: IORef Ingredients -> SandwichRecipe Sandwich -> IO Sandwich
makeSandwich ingredsRef recipe = interpretRecipe ingredsRef recipe
