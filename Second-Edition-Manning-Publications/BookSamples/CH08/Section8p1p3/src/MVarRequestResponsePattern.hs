module MVarRequestResponsePattern where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

type Request a = MVar a
type Response b = MVar b
type Pipe a b = (Request a, Response b)

createPipe :: IO (Pipe a b)
createPipe = do
    request <- newEmptyMVar
    response <- newEmptyMVar
    return (request, response)

sendRequest :: Pipe a b -> a -> IO b
sendRequest pipe@(request, response) a = do
    putMVar request a
    takeMVar response


worker :: Pipe a b -> (a -> b) -> IO ()
worker pipe@(request, response) f = do
    a <- takeMVar request
    putMVar response (f a)
    worker pipe f


{-
-- A better version of worker and listen: recursion is hidden.
listen :: Pipe a b -> (a -> b) -> IO ()
listen pipe@(request, response) f = do
    a <- takeMVar request
    let b = f a
    putMVar response b

worker :: Pipe a b -> (a -> b) -> IO ()
worker pipe f = forever (listen pipe f)
-}

fizzBuzz x | isDivided x 15 = "FizzBuzz"
           | isDivided x 5  = "Buzz"
           | isDivided x 3  = "Fizz"
           | otherwise      = show x

isDivided x n = (x `mod` n) == 0

fizzBuzzProcessor :: Pipe Int String -> IO ()
fizzBuzzProcessor pipe = worker pipe fizzBuzz


generator :: Pipe Int String -> Int -> IO ()
--generator pipe 3000000 = return ()
generator pipe i = do
    result <- sendRequest pipe i
    --when (i `isDivided` 100000) $ putStrLn ("[" ++ show i ++ "]: " ++ result)
    putStrLn ("[" ++ show i ++ "]: " ++ result)
    generator pipe (i + 1)
