module Main where

import Data.Char (toLower)

fizzBuzz :: Int -> String
fizzBuzz x | (x `mod` 15) == 0 = "FizzBuzz"
           | (x `mod` 5)  == 0 = "Buzz"
           | (x `mod` 3)  == 0 = "Fizz"
           | otherwise = show x

morseCode :: [(Char, String)]
morseCode =
  [ ('b', "-..."), ('f', "..-."), ('i', ".."), ('u', "..-")
  , ('z', "--.."), ('0', "-----"), ('1', ".----"), ('2', "..---")
  , ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-....")
  , ('7', "--..."), ('8', "---.."), ('9', "----.") ]

toMorse :: Char -> String
toMorse char = case lookup char morseCode of
    Just code -> code
    Nothing -> "???"

morseBelt :: Int -> [String]
morseBelt = map (' ' :) . map toMorse . map toLower . fizzBuzz

morseBelt' :: Int -> [String]
morseBelt' = map ((' ' :) . toMorse . toLower) . fizzBuzz

morseFizzBuzzes :: String
morseFizzBuzzes = (concat . concatMap morseBelt) [1..100]

main = do
    putStrLn morseFizzBuzzes
    
    putStrLn helloWorld1


prependSpace :: [Char] -> [Char]
prependSpace = (' ':)

appendWord :: Char -> [Char]
appendWord = (:"World")

helloWorld1 = "Hello," ++ (prependSpace "World")
helloWorld2 = "Hello," ++ (appendWord ' ')

hello = 'H' : 'e' : 'l' : 'l' : 'o' : []

-- all these functions have type (Int -> String)
toLowerFizzBuzz1 = map toLower . fizzBuzz -- eta-conversion
toLowerFizzBuzz2 n = (map toLower . fizzBuzz) n
toLowerFizzBuzz3 n = map toLower . fizzBuzz $ n
toLowerFizzBuzz4 n = map toLower (fizzBuzz n)
toLowerFizzBuzz5 n = map toLower $ fizzBuzz n
toLowerFizzBuzz6 n = map toLower $ fizzBuzz $ n

