module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever, void)
import           Data.IORef         (IORef, modifyIORef, newIORef, readIORef,
                                     writeIORef)
import           System.Random      (randomRIO)

data Meteor = Meteor
  { size :: Int
  , mass :: Int
  }
  deriving (Show, Read, Eq)

type ReportingChannel = IORef [Meteor]

getRandomMeteor :: IO Meteor
getRandomMeteor = do
  rndSize <- randomRIO (1, 100)
  rndMass <- randomRIO (rndSize, rndSize * 10)
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

  someRef <- newIORef [1..10]
  forever $ forkIO $ do
    val <- readIORef someRef
    print val

  threadDelay rndDelay

trackingCenter :: ReportingChannel -> IO ()
trackingCenter ch = do
  reported <- readIORef ch
  writeIORef ch []
  print $ "Reported: " <> show (length reported)
  threadDelay 10000

app :: IO ()
app = do
  ch <- newIORef []
  forkIO $ forever $ astronomer ch
  forever $ trackingCenter ch

main :: IO ()
main = app
