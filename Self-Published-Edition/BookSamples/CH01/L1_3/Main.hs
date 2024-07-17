module Main where

import qualified Native.Core.Thermometer as T
import qualified ServerContext.Connection as C

readThermometer :: String -> IO T.Temperature
readThermometer = T.read

sendTemperature :: String -> Float -> IO ()
sendTemperature = C.send "temperature"

readTemperature :: IO Float
readTemperature = do
    t1 <- readThermometer "T-201A"
    putStrLn "read and transformed"
    return $ case t1 of
        T.Kelvin  v -> 273.15 - v
        T.Celsius v -> v


readAndSend :: IO ()
readAndSend = do
    t1 <- readTemperature
    let t2 = t1 - 12.5 -- defect device!
    sendTemperature "T-201A" t2



main :: IO ()
main = readAndSend
