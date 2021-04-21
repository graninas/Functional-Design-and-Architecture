import Control.Monad.State

factorial n = let (fact, _) = go (1, n)
              in fact
  where  
    go (part, 0)       = (part, 0)
    go (part, counter) = go (part * counter, counter - 1)

factorialStateful :: Integer -> State Integer ()
factorialStateful 0 = return ()
factorialStateful n = do
    part <- get
    put (part * n)
    factorialStateful (n - 1)

printFactorial :: Integer -> IO ()
printFactorial n = do
    let fact = execState (factorialStateful n) 1
    print fact



    
factorialProducts :: Integer -> State [Integer] ()
factorialProducts 0 = return ()
factorialProducts counter = do
    (prevPart:parts) <- get
    let nextPart = prevPart * counter
    put (nextPart : prevPart : parts)
    factorialProducts (counter - 1)

printFactorialProducts :: Integer -> IO ()
printFactorialProducts n = do
    let products = execState (factorialProducts n) [1]
    print products
