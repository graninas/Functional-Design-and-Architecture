import Control.Concurrent.MVar

add :: MVar Int -> Int -> IO ()
add mVar val = do
    v <- takeMVar mVar
    putMVar mVar (v + val)

main = do
    mVar <- newMVar 10
    add mVar 50
    v <- takeMVar mVar
    print v    -- will print 60