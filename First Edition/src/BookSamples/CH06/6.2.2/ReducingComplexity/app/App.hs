module App where

import           Control.Monad
import           Data.IORef             (IORef)

import           Lang


data Meteor = Meteor
  { size :: Int
  , mass :: Int
  }
  deriving (Show, Read, Eq)

type ReportingChannel = IORef [Meteor]


-- Methods
forkProcess :: LangL () -> AppL ()
forkProcess = undefined
evalLang    :: LangL a  -> AppL a
evalLang    = undefined

getRandomMeteor :: LangL Meteor
getRandomMeteor = do
  rndSize <- getRandomInt (1, 100)
  rndMass <- getRandomInt (rndSize, rndSize * 10)
  pure $ Meteor rndSize rndMass

reportMeteor :: ReportingChannel -> Meteor -> LangL ()
reportMeteor ch meteor = do
  reported <- readVar ch
  writeVar ch $ meteor : reported

astronomer :: ReportingChannel -> LangL ()
astronomer ch = do
  rndMeteor <- getRandomMeteor
  rndDelay <- getRandomInt (1000, 10000)
  reportMeteor ch rndMeteor
  delay rndDelay

trackingCenter :: ReportingChannel -> LangL ()
trackingCenter ch = do
  reported <- readVar ch
  writeVar ch []
  delay 10000

app :: AppL ()
app = do
  ch <- evalLang $ newVar []
  forkProcess $ forever $ astronomer ch
  evalLang $ forever $ trackingCenter ch
