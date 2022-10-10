{-# LANGUAGE GADTs #-}
module Framework.Logger.Language where

import Data.Text
import Control.Monad.Free.Church


data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord)

type Message = Text

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
