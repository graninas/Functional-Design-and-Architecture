{-# LANGUAGE ExistentialQuantification #-}

module Control where

import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

import qualified ScriptingDSL as S

data Control a = forall b. EvalScript (S.Script b) (b -> a)

instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

type DeviceControl a = F.Free Control a

evalScript :: S.Script a -> DeviceControl a
evalScript scr = F.liftF (EvalScript scr id)

class Monad m => Interpreter m where
    onEvalScript :: S.Script b -> m b

interpret :: (Monad m, Interpreter m) => DeviceControl a -> m a
interpret (F.Pure a) = return a
interpret (F.Free (EvalScript s nextF)) = do
    v <- onEvalScript s
    interpret (nextF v)
