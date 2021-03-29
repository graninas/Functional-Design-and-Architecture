{-# LANGUAGE DeriveFunctor #-}
module InfrastructureDSL where

import Control.Monad.Free
import Prelude hiding (read)
import Types

type ValueSource = String
type Receiver = Value -> IO ()

logReceiver :: Receiver
logReceiver = \v -> print v

alarmReceiver = \v -> print ("WARNING!", v)

-- TODO: remove store reading from here to DataAccessScript
data Action a = StoreReading Reading a
              | SendTo Receiver Value a
              | GetCurrentTime (Time -> a)
  deriving (Functor)

type InfrastructureScript a = Free Action a

storeReading :: Reading -> InfrastructureScript ()
storeReading reading = liftF $ StoreReading reading ()

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())

logMsg :: String -> InfrastructureScript ()
logMsg = sendTo logReceiver . StringValue

alarm :: String -> InfrastructureScript ()
alarm = sendTo alarmReceiver . StringValue

getCurrentTime :: InfrastructureScript Time
getCurrentTime = liftF (GetCurrentTime id)

class Monad m => Interpreter m where
    onStoreReading :: Reading -> m ()
    onSendTo :: Receiver -> Value -> m ()
    onGetCurrentTime :: m Time

interpret :: (Monad m, Interpreter m) => InfrastructureScript a -> m a
interpret (Pure a) = return a
interpret (Free (StoreReading r next)) = do
    onStoreReading r
    interpret next
interpret (Free (SendTo r v next)) = do
    onSendTo r v
    interpret next
interpret (Free (GetCurrentTime nextF)) = do
    v <- onGetCurrentTime
    interpret (nextF v)
    
