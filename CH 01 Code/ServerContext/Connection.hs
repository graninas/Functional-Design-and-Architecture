module ServerContext.Connection where


send :: String -> String -> Float -> IO ()
send _ _ _ = putStrLn "Sended" >> return ()
