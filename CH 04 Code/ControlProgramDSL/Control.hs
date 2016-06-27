{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Control where

import Control.Monad.Trans.Free
import qualified Control.Monad.Free as F

import ScriptingDSL

data Control a = forall b. EvalScript (Script b) (b -> a)

instance Functor Control where
    fmap f (EvalScript scr g) = EvalScript scr (f . g)

type ControlProgram a = F.Free Control a

evalScript :: Script a -> ControlProgram a
evalScript scr = F.liftF (EvalScript scr id)





