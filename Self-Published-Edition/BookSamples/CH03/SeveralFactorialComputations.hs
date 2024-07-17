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
    
squareStateful :: Integer -> State Integer ()
squareStateful n = do
    put n
    multiply n
    
printValues :: IO ()
printValues = do
    print (execState (factorialStateful 10) 1)
    print (execState (squareStateful 5) 0)
    