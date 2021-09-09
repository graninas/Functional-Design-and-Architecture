module FizzBuzzMorse where

import Data.Char (toLower)

fizzBuzz :: Int -> String
fizzBuzz x | (x `mod` 15) == 0 = "FizzBuzz"
           | (x `mod` 5)  == 0 = "Buzz"
           | (x `mod` 3)  == 0 = "Fizz"
           | otherwise = show x

morseCode :: [(Char, String)]
morseCode =
  [ ('b', "-..."),  ('f', "..-."),  ('i', "..")
  , ('u', "..-"),   ('z', "--.."),  ('0', "-----")
  , ('1', ".----"), ('2', "..---"), ('3', "...--")
  , ('4', "....-"), ('5', "....."), ('6', "-....")
  , ('7', "--..."), ('8', "---.."), ('9', "----.")
  ]

toMorse :: Char -> String
toMorse char = case lookup char morseCode of
  Just code -> code
  Nothing   -> "???"

morseBelt :: Int -> [String]
morseBelt = map (' ' :)
          . map toMorse
          . map toLower
          . fizzBuzz

morseFizzBuzzes :: String
morseFizzBuzzes = (concat . concatMap morseBelt) [1..100]

-- Small excerpt from output:
-- .---- ..--- ..-. .. --.. --.. ....- -... ..-
-- --.. --.. ..-. .. --.. --.. --... ---..
