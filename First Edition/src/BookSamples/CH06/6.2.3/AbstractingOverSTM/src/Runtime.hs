module Runtime where

import           Control.Concurrent     (forkFinally, forkIO, threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM (STM, TVar, TMVar, modifyTVar, writeTVar, readTVar)
import           Control.Monad
import           Control.Monad.Free     (Free, foldFree)
import qualified Data.Map               as Map
import           GHC.Exts

import           Lang

newtype VarHandle = VarHandle (TVar Any)  -- Untyped data stored in here

data StateRuntime = StateRuntime
    { _varId  :: TVar VarId                   -- ^ Var id counter
    , _state  :: TMVar (Map.Map VarId VarHandle)  -- ^ Node state
    }

data Runtime = Runtime
  { _maxThreadsCount :: Int
  , _curThreadsCount :: TVar Int
  , _stateRuntime :: StateRuntime
  }

  -- | Interpret StateF as STM.
interpretStateF :: StateRuntime -> StateF a -> STM a
interpretStateF stateRt (NewVar  val next     )  = undefined
interpretStateF stateRt (ReadVar var next     )  = undefined
interpretStateF stateRt (WriteVar var val next)  = undefined
interpretStateF _       (Retry                )  = undefined

runStateL :: StateRuntime -> StateL a -> STM a
runStateL stateRt = foldFree (interpretStateF stateRt)


-- Function to run LangL script
runLangL :: Runtime -> LangL a -> IO a
runLangL = undefined

-- Increasing the counter thread safely
increaseProcessCount :: Int -> TVar Int -> STM ()
increaseProcessCount maxCount psVar = do
  ps <- readTVar psVar
  when (ps == maxCount) STM.retry
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
  STM.atomically $ increaseProcessCount maxPs psVar

  void $ forkFinally
    (runLangL rt act)
    (const $ STM.atomically $ decreaseProcessCount psVar)
  pure $ next ()

runAppL :: Runtime -> AppL a -> IO a
runAppL rt = foldFree (interpretAppF rt)
