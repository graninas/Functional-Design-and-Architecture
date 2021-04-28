module Main where

import qualified Native.Core.Thermometer as T
import qualified ServerContext.Connection as C

type DeviceName = String
type DataType = String
type TransformF a = Float -> ActionDsl a

data ActionDsl a
    = ReadDevice DeviceName (a -> ActionDsl a)
    | Transform (a -> Float) a (TransformF a)
    | Correct (Float -> Float) Float (TransformF a)
    | Send DataType DeviceName Float

therm = "T-201A"
temp = "temperature"

transform (T.Kelvin  v) = v - 273.15
transform (T.Celsius v) = v

correction v = v - 12.5

scenario :: ActionDsl T.Temperature
scenario =
    ReadDevice therm (\v ->
    Transform transform v (\v1 ->
    Correct correction v1 (\v2 ->
    Send temp therm v2)))

interpret :: ActionDsl T.Temperature -> IO ()
interpret (ReadDevice n a) = do
    v <- T.read n
    interpret (a v)
interpret (Transform f v a) = interpret (a (f v))
interpret (Correct f v a)   = interpret (a (f v))
interpret (Send t n v)      = C.send t n v

readAndSend :: IO ()
readAndSend = interpret scenario


main :: IO ()
main = readAndSend
