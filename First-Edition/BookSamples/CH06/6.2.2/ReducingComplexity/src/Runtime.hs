module Runtime where

import           Control.Concurrent     (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Free     (Free, foldFree, liftF)
import           Data.IORef             (IORef, modifyIORef, newIORef,
                                         readIORef, writeIORef)

import Lang

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
