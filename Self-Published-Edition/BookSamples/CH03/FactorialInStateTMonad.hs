import Control.Monad.Trans.State
import Control.Monad.Trans (lift)

type StateIO state returnval = StateT state IO returnval

factorialStateful :: Integer -> StateIO Integer ()
factorialStateful 0 = return ()
factorialStateful n = do
    part <- get
    lift (print part)
    put (part * n)
    factorialStateful (n - 1)

printFactorial :: Integer -> IO ()
printFactorial n = evalStateT (factorialStateful n) 1
