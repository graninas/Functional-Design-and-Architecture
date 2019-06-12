{-# LANGUAGE GADTs #-}

module ThreadBookkeeping where

import           Control.Concurrent     (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free     (Free, foldFree, liftF)
import           Data.IORef             (IORef, modifyIORef, newIORef,
                                         readIORef, writeIORef)
import qualified Data.Set               as Set
import           System.Random          (randomRIO)

data Meteor = Meteor
  { size :: Int
  , mass :: Int
  }
  deriving (Show, Read, Eq)

type ReportingChannel = IORef [Meteor]

-- Lower layer of business logic, the LangL eDSL

data LangF next where
  Delay        :: Int -> (() -> next)          -> LangF next
  GetRandomInt :: (Int, Int) -> (Int -> next)  -> LangF next
  NewVar       :: a -> (IORef a -> next)       -> LangF next
  ReadVar      :: IORef a -> (a -> next)       -> LangF next
  WriteVar     :: IORef a -> a -> (() -> next) -> LangF next

instance Functor LangF where
  fmap f (Delay        t       next) = Delay        t       (f . next)
  fmap f (GetRandomInt range   next) = GetRandomInt range   (f . next)
  fmap f (NewVar       val     next) = NewVar       val     (f . next)
  fmap f (ReadVar      var     next) = ReadVar      var     (f . next)
  fmap f (WriteVar     var val next) = WriteVar     var val (f . next)


type LangL = Free LangF

-- Methods
delay        :: Int          -> LangL ()
delay        = undefined
getRandomInt :: (Int, Int)   -> LangL Int
getRandomInt = undefined
newVar       :: a            -> LangL (IORef a)
newVar       = undefined
readVar      :: IORef a      -> LangL a
readVar      = undefined
writeVar     :: IORef a -> a -> LangL ()
writeVar     = undefined

-- Upper layer of business logic, the AppL eDSL

data AppF next where
  EvalLang    :: LangL a  -> (a -> next)  -> AppF next
  ForkProcess :: LangL () -> (() -> next) -> AppF next

instance Functor AppF where
  fmap f (EvalLang    act next) = EvalLang    act (f . next)
  fmap f (ForkProcess act next) = ForkProcess act (f . next)

type AppL = Free AppF

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

data Runtime = Runtime
  { _maxThreadsCount :: Int
  , _curThreadsCount :: TVar Int
  }

-- Function to run LangL script
runLangL :: Runtime -> LangL a -> IO a
runLangL = undefined

-- Increasing the counter thread safely
increaseProcessCount :: Int -> TVar Int -> STM ()
increaseProcessCount maxCount psVar = do
  ps <- readTVar psVar
  when (ps == maxCount) retry
  writeTVar psVar $ ps + 1

-- Decreasing the counter thread safely
decreaseProcessCount :: TVar Int -> STM ()
decreaseProcessCount psVar = modifyTVar psVar (\x -> x - 1)

-- TODO: use STM
-- Interpreting the AppF language.
interpretAppF :: Runtime -> AppF a -> IO a
interpretAppF rt (EvalLang act next) = do
  r <- runLangL rt act
  pure $ next r
interpretAppF rt (ForkProcess act next) = do
  let psVar = _curThreadsCount rt
  let maxPs = _maxThreadsCount rt
  atomically $ increaseProcessCount maxPs psVar
  void $ forkFinally
    (runLangL rt act)
    (const $ atomically $ decreaseProcessCount psVar)
  pure $ next ()

runAppL :: Runtime -> AppL a -> IO a
runAppL rt = foldFree (interpretAppF rt)
