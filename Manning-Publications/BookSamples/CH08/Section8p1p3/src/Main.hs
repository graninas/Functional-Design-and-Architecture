module Main where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import MVarRequestResponsePattern




main :: IO ()
main = do
  pipe <- createPipe
  _ <- forkIO (fizzBuzzProcessor pipe)
  _ <- forkIO (generator pipe 0)
  pure ()
