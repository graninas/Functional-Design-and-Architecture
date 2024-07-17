module Main where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans (lift)

data Config = Config { caseIgnoring :: Bool, normalization :: Bool }
type WordStatistics = Map.Map String Int
type WordStatStack a = ReaderT Config (StateT WordStatistics IO) a

countWord :: String -> WordStatistics -> WordStatistics
countWord w stat = Map.insertWith (+) w 1 stat

collectStats :: [String] -> WordStatStack WordStatistics
collectStats ws = lift $ mapM_ (modify.countWord) ws >> get

tokenize :: String -> WordStatStack [String]
tokenize txt = do
    Config ignore norm <- ask
    
    let normalize ch = if isAlpha ch then ch else ' '
    let transform1 = if ignore then map toLower else id
    let transform2 = if norm then map normalize else id
    
    lift . lift $ print ("Ignoring case: " ++ show ignore)
    lift . lift $ print ("Normalize: " ++ show norm)
    
    return . words . transform2 . transform1 $ txt

calculateStats :: String -> WordStatStack WordStatistics
calculateStats txt = do
    wordTokens <- tokenize txt
    collectStats wordTokens
    
main :: IO ()
main = do
    let text = "To be, or not to be: that is the question."
    let config = Config True True
    let runTopReaderMonad = runReaderT (calculateStats text) config
    let runMiddleStateMonad = execStateT runTopReaderMonad Map.empty
    let printStat (w, cnt) = print (w ++ ": " ++ show cnt)
    stats <- runMiddleStateMonad
    mapM_ printStat (Map.toAscList stats)
    