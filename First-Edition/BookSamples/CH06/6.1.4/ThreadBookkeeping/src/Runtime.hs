module Runtime where

import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Free (foldFree)

import Lang

data Runtime = Runtime
  { _maxThreadsCount :: Int
  , _curThreadsCount :: MVar Int
  }

-- Function to run LangL script
runLangL :: Runtime -> LangL a -> IO a
runLangL = undefined

-- Decreasing the counter thread safely
decreaseProcessCount :: MVar Int -> IO ()
decreaseProcessCount psVar = do
  ps <- takeMVar psVar
  putMVar psVar $ ps - 1

-- TODO: use STM
-- Interpreting the AppF language.
interpretAppF :: Runtime -> AppF a -> IO a
interpretAppF rt (EvalLang act next) = do
  r <- runLangL rt act
  pure $ next r
interpretAppF rt (ForkProcess act next) = do
  go 1
  pure $ next ()
  where
    go factor = do
      let psVar = _curThreadsCount rt
      let maxPs = _maxThreadsCount rt
      ps <- takeMVar psVar

      when (ps == maxPs) $ do
        putMVar psVar ps
        threadDelay $ 10 ^ factor
        go $ factor + 1

      when (ps /= maxPs) $ do
        putMVar psVar $ ps + 1
        void $ forkFinally
          (runLangL rt act)
          (const $ decreaseProcessCount psVar)

runAppL :: Runtime -> AppL a -> IO a
runAppL rt = foldFree (interpretAppF rt)
