module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import           App
import           Lang
import           Runtime

main :: IO ()
main = do
  psVar <- STM.newTVarIO 0
  varIdVar <- STM.newTVarIO 0
  stVar <- STM.newTMVarIO Map.empty

  let rt = Runtime 4 psVar $ StateRuntime varIdVar stVar
  runAppL rt app
