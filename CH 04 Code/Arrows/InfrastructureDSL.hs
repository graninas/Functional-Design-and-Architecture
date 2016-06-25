{-# LANGUAGE DeriveFunctor #-}
module InfrastructureDSL where

import Control.Monad.Free
import Prelude hiding (read)

import ControllerDSL

type ValueSource = String
type Receiver = Value -> IO ()

type DbValue = (ValueSource, Measurement, (String, Float))

-- TODO: can be storing of value unified with SendTo?
data Action a = StoreValue DbValue a
              | SendTo Receiver Value a
  deriving (Functor)

type InfrastructureScript a = Free Action a

storeValue :: DbValue -> InfrastructureScript ()
storeValue val = liftF $ StoreValue val ()

sendTo :: Receiver -> Value -> InfrastructureScript ()
sendTo r v = liftF (SendTo r v ())