module Main where

--import Control.Monad.State
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

type FactorialStateIOMonad a = StateT CalcParameters IO a

printResult :: Integer -> Integer -> IO ()
printResult i fact = print ("[" ++ show i ++ "] " ++ show fact)

calcFactorialStatefulIO :: FactorialStateIOMonad Integer
calcFactorialStatefulIO = do
    (i, fact) <- get
    if (i <= 0)
    then return fact
    else do
        lift (printResult i fact)        
        put (i - 1, fact * i)
        calcFactorialStatefulIO

factFor n = (n, 1)

calculate :: IO (Integer, (Integer, Integer))
calculate = runStateT calcFactorialStatefulIO (10, 1)

main :: IO ()
main = do
    
--    let (fact1, _) = runState calcFactorialStateful (10, 1)
--    print fact1
    
    (fact, _) <- calculate
    print fact

