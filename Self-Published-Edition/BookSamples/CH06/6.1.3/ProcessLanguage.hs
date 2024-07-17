module Main where

import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.Random (randomRIO)

data Meteor = Meteor
  { size :: Int
  , mass :: Int
  }
  deriving (Show, Read, Eq)

type ReportingChannel = IORef [Meteor]

getRandomMeteor = do
  rndSize <- randomRIO (1, 100)
  rndMass <- randomRIO (rndSize * 1, rndSize * 10)
  pure $ Meteor rndSize rndMass

reportMeteor :: ReportingChannel -> Meteor -> IO ()
reportMeteor ch meteor = do
  reported <- readIORef ch
  writeIORef ch $ meteor : reported

astronomer :: ReportingChannel -> IO ()
astronomer ch = do
  rndMeteor <- getRandomMeteor
  rndDelay <- randomRIO (1000, 10000)
  reportMeteor ch rndMeteor
  threadDelay rndDelay

trackingCenter :: ReportingChannel -> IO ()
trackingCenter ch = do
  reported <- readIORef ch
  writeIORef ch []
  threadDelay 10000

main :: IO ()
main = do
  ch <- newIORef []
  forkIO $ forever $ astronomer ch
  forever $ trackingCenter ch
