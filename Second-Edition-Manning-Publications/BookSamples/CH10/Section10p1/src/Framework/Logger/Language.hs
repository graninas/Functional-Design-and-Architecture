{-# LANGUAGE GADTs #-}
module Framework.Logger.Language where

import Data.Text
import Control.Monad.Free.Church

import Framework.Logger.Types


data LoggerF next where
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
