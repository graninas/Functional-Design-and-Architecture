module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import qualified Data.Map as Map

type Data = Map.Map Int String
type StateIO = StateT Data IO
type MaybeStateIO a = MaybeT StateIO a

calculations :: MaybeStateIO ()
calculations = do
    lift (lift (putStrLn "bla-bla"))
    lift (modify (Map.insert 3 "3"))
    lift (modify (Map.insert 1 "1"))
    mb <- lift (get >>= (return . Map.lookup 1))
    lift (lift (print mb))

main = runStateT (runMaybeT calculations) Map.empty



