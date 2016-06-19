module Main where

import Control.Monad.Trans.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad


-- State monad
type CalcParameters = (Integer, Integer)
type FactorialStateMonad a = State CalcParameters a

calcFactorialStateful :: FactorialStateMonad Integer
calcFactorialStateful = do
    (i, fact) <- get
    if (i <= 1)
        then return fact
        else do 
            put (i - 1, fact * i)
            calcFactorialStateful

-- Reader monad

newtype Select = Select String
newtype From   = From String
newtype Where  = Where String
data SqlDefinition = SqlDef Select From Where

data Environment = Env {
    sqlDefinition :: SqlDefinition,
    optimizeQuery :: Bool,
    useJoins :: Bool,
    prettyPrint :: Bool }
type SqlQueryGenMonad a = Reader Environment a

sqlScriptGen :: SqlQueryGenMonad String
sqlScriptGen = do
    Env def opt joins _ <- ask
    if opt then return (genOptimal def)
           else return (getSimple  def)
  where
      genOptimal _ = "<<optimized sql>>"
      getSimple  _ = "<<simple sql>>"


-- Writer monad
data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String
  deriving (Show)
    
data Event = NewValue Int Value
           | ModifyValue Int Value Value
           | DeleteValue Int
  deriving (Show)

type EventSourcingMonad a = Writer [Event] a

valuesToString :: [Value] -> EventSourcingMonad String
valuesToString vals = do
    mapM_ addValueEvent  (zip [1..] vals)
    let strings = map toString vals
    return (concat strings)
  where toString :: Value -> String
        toString (BoolValue b)   = show b
        toString (IntValue i)    = show i
        toString (FloatValue f)  = show f
        toString (StringValue s) = s
        addValueEvent :: (Int, Value) -> EventSourcingMonad ()
        addValueEvent (i, v) = tell [NewValue i v]
        
type IOMonad a = IO a
askAndPrint :: IOMonad ()
askAndPrint = do
    putStrLn "Print something:"
    line <- getLine
    putStrLn "You printed:"
    putStrLn line


sqlDef = SqlDef (Select "field") 
                (From   "table")
                (Where  "id > 10")

sqlGenSettings = Env sqlDef True True True

values = [ BoolValue True
         , FloatValue 10.3
         , FloatValue 522.643]
         
factFor n = (n, 1)

valuesToStringCalc :: EventSourcingMonad String
valuesToStringCalc = valuesToString values

calculateStuff :: String
calculateStuff = let
    (fact, _) = runState  calcFactorialStateful (10, 1)
    sql       = runReader sqlScriptGen sqlGenSettings
    (s, es)   = runWriter valuesToStringCalc
    in "\nfact: " ++ show fact ++
       "\nsql: "  ++ show sql ++
       "\nvalues string: " ++ s ++
       "\nevents: " ++ show es
    
    
main = do
    
    let fact   = runState  calcFactorialStateful (10, 1)
    let sql    = runReader sqlScriptGen        sqlGenSettings
    let (s, e) = runWriter (valuesToString values)

    let (fact1, _) = runState calcFactorialStateful (10, 1)
    print fact1

    putStrLn calculateStuff

    print "Done."

