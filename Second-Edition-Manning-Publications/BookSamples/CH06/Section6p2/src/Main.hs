module Main where

import Sandwich.PureInterpreter
import Sandwich.Language

import qualified Data.Map as Map

mySandwich :: SandwichRecipe Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  body3 <- addComponent Salt body2
  finishSandwich Nothing body3

main :: IO ()
main = do
  let ingreds = Map.fromList
        [ (Tomato, 10)
        , (Bread Toast, 6)
        , (Salt, 4)
        , (Cheese, 5)
        ]

  let eSandwich = makeSandwich ingreds mySandwich
  case eSandwich of
    Left err -> print err
    Right sandwich -> print sandwich
