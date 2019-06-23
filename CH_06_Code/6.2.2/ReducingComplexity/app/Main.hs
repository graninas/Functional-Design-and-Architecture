module Main where

import           Control.Concurrent.STM

import           App
import           Lang
import           Runtime

main :: IO ()
main = do
  psVar <- newTVarIO 0
  let rt = Runtime 4 psVar
  runAppL rt app
