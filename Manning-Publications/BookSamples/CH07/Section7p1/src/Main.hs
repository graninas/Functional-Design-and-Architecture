module Main where

import Sandwich.ImpureInterpreter
import Sandwich.Language

import qualified Data.Map as Map
import Data.IORef 

-- Business logic
mySandwich :: SandwichRecipe Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  body3 <- addComponent Salt body2
  finishSandwich Nothing body3

-- Operational data
ingredients :: Ingredients
ingredients = Map.fromList
  [ (Tomato, 10), (Bread Toast, 6)
  , (Salt, 4), (Cheese, 5) ]

-- Application
main :: IO ()
main = do
  -- sudo make me a sandwich
  ingredientsRef <- newIORef ingredients
  sandwich <- interpretRecipe ingredientsRef mySandwich
  print sandwich
