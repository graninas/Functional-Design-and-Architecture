module Main where

import           Control.Concurrent.STM

import           ThreadBookkeeping

main :: IO ()
main = do
  psVar <- newTVarIO 0
  let rt = Runtime 4 psVar
  runAppL rt app
