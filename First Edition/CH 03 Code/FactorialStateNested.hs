import Control.Monad.State


multiply :: Integer -> State Integer ()
multiply n = do
    value <- get
    put (value * n)

factorialStateful :: Integer -> State Integer ()
factorialStateful 0 = return ()
factorialStateful n = do
    multiply n
    factorialStateful (n - 1)
    
printFactorial :: Integer -> IO ()
printFactorial n = do
    print (execState (factorialStateful n) 1)