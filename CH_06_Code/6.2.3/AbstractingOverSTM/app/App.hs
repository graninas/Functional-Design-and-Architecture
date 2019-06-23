{-# LANGUAGE GADTs #-}

module App where

import           Control.Monad

import           Lang

data Meteor = Meteor
  { size :: Int
  , mass :: Int
  }
  deriving (Show, Read, Eq)


type ReportingChannel = StateVar [Meteor]

getRandomMeteor :: LangL Meteor
getRandomMeteor = do
  rndSize <- getRandomInt (1, 100)
  rndMass <- getRandomInt (rndSize, rndSize * 10)
  pure $ Meteor rndSize rndMass

reportMeteor' :: ReportingChannel -> Meteor -> StateL ()
reportMeteor' ch meteor = do
  reported <- readVar ch
  writeVar ch $ meteor : reported

-- Evaluates StateL script with own `atomically` function.
reportMeteor :: ReportingChannel -> Meteor -> LangL ()
reportMeteor ch meteor = atomically $ reportMeteor' ch meteor

astronomer :: ReportingChannel -> LangL ()
astronomer ch = do
  rndMeteor <- getRandomMeteor
  rndDelay <- getRandomInt (1000, 10000)
  reportMeteor ch rndMeteor
  delay rndDelay

trackingCenter :: ReportingChannel -> LangL ()
trackingCenter ch = do
  reported <- atomically $ readVar ch
  atomically $ writeVar ch []
  delay 10000

app :: AppL ()
app = do
  ch <- evalLang $ atomically $ newVar []
  forkProcess $ forever $ astronomer ch
  evalLang $ forever $ trackingCenter ch
