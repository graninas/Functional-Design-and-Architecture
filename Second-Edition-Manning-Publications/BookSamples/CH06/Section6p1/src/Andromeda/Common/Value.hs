module Andromeda.Common.Value   -- hierarchical module name
  ( -- export list:
    Value,            -- type without value constructors
    boolValue,        -- smart constructors
    stringValue,
    intValue,
    floatValue
  ) where

-- data type Value and smart constructors go here



data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String




boolValue :: Bool -> Value
boolValue b = BoolValue b

stringValue :: String -> Value
stringValue s = StringValue s

intValue :: Int -> Value
intValue i = IntValue i

floatValue :: Float -> Value
floatValue f = FloatValue f
