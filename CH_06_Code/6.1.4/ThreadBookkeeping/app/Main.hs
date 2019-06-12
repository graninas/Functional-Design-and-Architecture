module Main where

import Control.Concurrent.MVar

import ThreadBookkeeping

main :: IO ()
main = do
  psVar <- newMVar 0
  let rt = Runtime 4 psVar
  runAppL rt app
