{-# LANGUAGE FlexibleInstances #-}
module ControllerDSL where

import Control.Monad.Free
import qualified Control.Monad.State as S (liftIO)
import qualified Control.Monad.Trans.State as S
import Prelude hiding (read)
import qualified Data.Map as M

-- These types are defined in a separate library
data Value = FloatValue Float
           | IntValue Int
           | StringValue String
           | BoolValue Bool
           | ListValue [Value]
  deriving (Show)
  
data Measurement = Measurement Value
  deriving (Show)
data Parameter = Temperature | Pressure
  deriving (Show)
  
-- Dummy types, should be designed later
data Property = Version | Status | SensorsList
  deriving (Show)
data Command = Command String
  deriving (Show)
type CommandResult = Either String String
data Controller = Controller String
  deriving (Show)
type SensorIndex = String
  
-- Parametrized type for a free eDSL
data Procedure a
    = Set Controller Property Value a
    | Get Controller Property (Value -> a)
    | Read Controller SensorIndex Parameter (Measurement -> a)
    | Run Controller Command (CommandResult -> a)
    
instance Functor Procedure where
    fmap g (Set c p v next) = Set c p v (g next)
    fmap g (Get c p f) = Get c p (g . f)
    fmap g (Read c si p f) = Read c si p (g . f)
    fmap g (Run c cmd f) = Run c cmd (g . f)

type ControllerScript a = Free Procedure a

-- Smart constructors:

get :: Controller -> Property -> ControllerScript Value
get c p = Free (Get c p Pure)

set :: Controller -> Property -> Value -> ControllerScript ()
set c p v = Free (Set c p v (Pure ()))

read :: Controller -> SensorIndex -> Parameter -> ControllerScript Measurement
read c si p = Free (Read c si p Pure)

run :: Controller -> Command -> ControllerScript CommandResult
run c cmd = Free (Run c cmd Pure)

class Monad m => Interpreter m where
    onSet  :: Controller -> Property -> Value -> m ()
    onGet  :: Controller -> Property -> m Value
    onRead :: Controller -> SensorIndex -> Parameter -> m Measurement
    onRun  :: Controller -> Command -> m CommandResult
    

interpret :: (Monad m, Interpreter m) => ControllerScript a -> m a
interpret (Pure a) = return a
interpret (Free (Get c p next)) = do
    v <- onGet c p
    interpret (next v)
interpret (Free (Set c p v next)) = do
    onSet c p v
    interpret next
interpret (Free (Read c si p next)) = do
    v <- onRead c si p
    interpret (next v)
interpret (Free (Run c cmd next)) = do
    v <- onRun c cmd
    interpret (next v)

temperature = Temperature
pressure = Pressure

toKelvin (Measurement (FloatValue v)) = Measurement . FloatValue $ v + 273.15
        