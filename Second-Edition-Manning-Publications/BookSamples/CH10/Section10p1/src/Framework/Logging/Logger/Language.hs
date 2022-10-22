{-# LANGUAGE GADTs #-}
module Framework.Logging.Logger.Language where

import Data.Text
import Control.Monad.Free.Church

import Framework.Logging.Types


-- | Logger interface
data LoggerF next where
  -- | Logging method.
  LogMessage
    :: LogLevel
    -> Message
    -> (() -> next)
    -> LoggerF next


type LoggerL = F LoggerF

instance Functor LoggerF where
  fmap f (LogMessage lvl msg next) = LogMessage lvl msg (f . next)

logMessage :: LogLevel -> Message -> LoggerL ()
logMessage level msg = liftF (LogMessage level msg id)

