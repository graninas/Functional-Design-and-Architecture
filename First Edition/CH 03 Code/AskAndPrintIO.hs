module Main where

askAndPrint :: IO ()
askAndPrint = do
    _ <- putStrLn "Print something:"
    line <- getLine
    putStrLn "You printed:"
    putStrLn line

askAndPrintTwice :: IO ()
askAndPrintTwice = do
    putStrLn "1st try:"
    askAndPrint
    putStrLn "2nd try:"
    askAndPrint

ask :: IO ()
ask = putStrLn "Print something:"

quote :: String -> String -- Pure function over String value
quote line = "'" ++ line ++ "'"

askAndQuote :: IO String
askAndQuote = do
    _ <- ask -- Explicitly discard the value ()
    line <- getLine
    return (quote line)

askQuoteAndPrint :: IO ()
askQuoteAndPrint = do
    val <- askAndQuote -- bind quoted line with val variable.
    putStrLn val -- val variable has String type.
    
main = askAndPrint
