{-# LANGUAGE FlexibleInstances #-}
module ControllerDSL where

import Control.Monad.Free
import qualified Control.Monad.State as S
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
    onSet _ _ _  = return ()
    onGet _ _    = return (StringValue "")
    onRead _ _ _ = return (Measurement (FloatValue 0.0))
    onRun _ _    = return (Left "Unknown")
    

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

        
        
controller :: Controller
controller = Controller "test"
sensor :: SensorIndex
sensor = "thermometer 00:01"

process :: Value -> ControllerScript String
process (StringValue "1.0") = do
    temp <- read controller sensor Temperature
    return (show temp)
process (StringValue v) = return ("Not supported: " ++ v)
process _ = error "Value type mistmatch."

script :: ControllerScript String
script = do
    v <- get controller Version
    process v
   
    
interpret' (Pure a) = return a
interpret' (Free (Set c p v next))   = print c >> print v >> print p >> interpret' next
interpret' (Free (Get c p next))     = print c >> print p >> interpret' (next (StringValue "1.0"))
interpret' (Free (Read c si p next)) = print c >> print si >> print p >> interpret' (next (toKelvin 1.1))
interpret' (Free (Run c cmd next))   = print c >> print cmd >> interpret' (next (Right "OK."))

{- Impure interpreter in the IO monad that prints every instruction
   with parameters. It also returns some dummy values
   for Get, Read and Run instructions. -}
   
interpret'' :: ControllerScript a -> IO a
interpret'' (Pure a) = return a
interpret'' (Free (Set c p v next)) = do
    print ("Set", c, v, p)
    interpret'' next
interpret'' (Free (Get c p next)) = do
    print ("Get", c, p)
    interpret'' (next (StringValue "1.0"))
interpret'' (Free (Read c si p next)) = do
    print ("Read", c, si, p)
    interpret'' (next (toKelvin 1.1))
interpret'' (Free (Run c cmd next)) = do
    print ("Run", c, cmd)
    interpret'' (next (Right "OK."))

{-
interpret (Pure a) = return a
interpret (Free (Set c p v next))   = print "Set" >> interpret' next
interpret (Free (Get c p next))     = print "Get" (next (StringValue "1.0"))
interpret (Free (Read c si p next)) = print c >> print si >> print p >> interpret' (next (toKelvin 1.1))
interpret (Free (Run c cmd next))   = print c >> print cmd >> interpret' (next (Right "OK."))
-}
    
type Key = (Controller, String)
type Mock = M.Map Key Value
type MockStateM a = S.State Mock a

type MockIOStateM = S.StateT Mock IO

emptyMock :: Mock
emptyMock = M.empty

toKelvin v = Measurement (FloatValue v)

instance Interpreter MockIOStateM where
    onSet  c p v  = S.liftIO (print c >> print v >> print p >> return ())
    onGet  c p    = S.liftIO (print c >> print p >> return (StringValue "1.0"))
    onRead c si p = S.liftIO (print c >> print si >> print p >> return (toKelvin 1.1))
    onRun  c cmd  = S.liftIO (print c >> print cmd >> return (Right "OK."))

test :: IO ()
test = do
    r1 <- interpret'' script
    r2 <- S.evalStateT (interpret script) emptyMock
    print r1
    print r2
    print (r1 == r2)
    
{-
mkKey a b = show a ++ show b

interpret :: ControllerScript () -> 
interpret (Pure ()) = return ()
interpret (Free (Get c p next)) = do
    let k = mkKey c p
    state <- get
    M.lookup k state
    
interpret (Free (Set c p v next))   = ...
interpret (Free (Read c si p next)) = ...
interpret (Free (Run c cmd next))   = ...
-}