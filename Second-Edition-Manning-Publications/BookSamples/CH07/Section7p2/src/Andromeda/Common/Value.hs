module Andromeda.Common.Value
  ( Value,
    boolValue,
    stringValue,
    intValue,
    floatValue
  ) where


data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String
  deriving (Show, Eq, Ord)


boolValue :: Bool -> Value
boolValue b = BoolValue b

stringValue :: String -> Value
stringValue s = StringValue s

intValue :: Int -> Value
intValue i = IntValue i

floatValue :: Float -> Value
floatValue f = FloatValue f
