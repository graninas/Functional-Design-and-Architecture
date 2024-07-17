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

        
        
controller :: Controller
controller = Controller "test"
sensor :: SensorIndex
sensor = "thermometer 00:01"
version = Version
temperature = Temperature

process :: Value -> ControllerScript String
process (StringValue "1.0") = do
    temp <- read controller sensor temperature
    return (show temp)
process (StringValue v) = return ("Not supported: " ++ v)
process _ = error "Value type mismatch."

script :: ControllerScript String
script = do
    v <- get controller version
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

type Mock = M.Map String Value
type MockState = S.State Mock
type MockIOState = S.StateT Mock IO

emptyMock :: Mock
emptyMock = M.empty

toKelvin v = Measurement (FloatValue v)
mkKey a b = show (a, b)
toMeasurement Pressure v = Measurement v
toMeasurement Temperature v = Measurement v

-- Stateful mocking interpreter
instance Interpreter MockState where
    onSet c prop v = do
        let key = mkKey c prop
        state <- S.get
        S.put (M.insert key v state)
    onGet c prop = do
        let key = mkKey c prop
        state <- S.get
        case M.lookup key state of
             Nothing -> error ("No property was set for " ++ key)
             Just v -> return v
    onRead c si par = do
        let key = mkKey (mkKey c par) si
        state <- S.get
        case M.lookup key state of
             Nothing -> error ("No readings available for " ++ key)
             Just v -> return (toMeasurement par v)
    onRun c cmd = do
        let key = mkKey c cmd
        state <- S.get
        case M.lookup key state of
             Nothing -> error ("Command not supported: " ++ key)
             Just v -> return (Right (show v))

-- Impure stateful interpreter
instance Interpreter MockIOState where
    onSet  c prop v = S.liftIO (print c >> print v >> print prop >> return ())
    onGet  c prop   = S.liftIO (print c >> print prop >> return (StringValue "1.0"))
    onRead c si par = S.liftIO (print c >> print si >> print par >> return (toKelvin 1.1))
    onRun  c cmd    = S.liftIO (print c >> print cmd >> return (Right "OK."))

-- Just impure interpreter
instance Interpreter IO where
    onSet c prop v = print ("Set", c, v, prop)
    onGet c prop = do
        print ("Get", c, prop)
        return (StringValue "1.0")
    onRead c si par = do
        print ("Read", c, si, par)
        return (toKelvin 1.1)
    onRun c cmd = do
        print ("Run", c, cmd)
        return (Right "OK.")

mock :: Mock
mock = M.fromList
    [ (mkKey controller version, StringValue "1.0")
    , (mkKey (mkKey controller temperature) sensor, FloatValue 33.1)]
        
scriptTest = do
    let (r, s) = S.runState (interpret script) mock
    print r
    print s
    
interpretersTest :: IO ()
interpretersTest = do
    r1 <- interpret'' script
    r2 <- S.evalStateT (interpret script) emptyMock
    print r1
    print r2
    print (r1 == r2)
    
test :: IO String
test = interpret script
    
