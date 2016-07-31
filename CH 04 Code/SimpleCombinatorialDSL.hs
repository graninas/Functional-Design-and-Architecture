
-- Dummy types
type DiffTime = Int
data Controller = Controller
data Parameter = Parameter
data Value = Value

-- Functions from system libraries:
threadDelay :: DiffTime -> IO ()
threadDelay = undefined

-- replicate :: Int -> a -> [a]
-- sequence :: Monad m => [m a] -> m [a]
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Function from hardware-related library:
readDevice :: Controller -> Parameter -> IO Value
readDevice = undefined

-- eDSL:
delay dt value = do
    threadDelay dt
    return value

times n f = sequence (replicate n f)

-- Script:
readValues dt n controller param = times n (reader >>= delayer)
    where
        reader = readDevice controller param
        delayer = delay dt