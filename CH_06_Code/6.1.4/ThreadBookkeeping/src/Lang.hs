{-# LANGUAGE GADTs #-}

module Lang where

import Data.IORef (IORef)
import Control.Monad
import Control.Monad.Free (Free, liftF)

-- Lower layer of business logic, the LangL eDSL

data LangF next where
  Delay        :: Int -> (() -> next)          -> LangF next
  GetRandomInt :: (Int, Int) -> (Int -> next)  -> LangF next
  NewVar       :: a -> (IORef a -> next)       -> LangF next
  ReadVar      :: IORef a -> (a -> next)       -> LangF next
  WriteVar     :: IORef a -> a -> (() -> next) -> LangF next

instance Functor LangF where
  fmap f (Delay        t       next) = Delay        t       (f . next)
  fmap f (GetRandomInt range   next) = GetRandomInt range   (f . next)
  fmap f (NewVar       val     next) = NewVar       val     (f . next)
  fmap f (ReadVar      var     next) = ReadVar      var     (f . next)
  fmap f (WriteVar     var val next) = WriteVar     var val (f . next)


type LangL = Free LangF

-- Methods
delay        :: Int          -> LangL ()
delay        = undefined
getRandomInt :: (Int, Int)   -> LangL Int
getRandomInt = undefined
newVar       :: a            -> LangL (IORef a)
newVar       = undefined
readVar      :: IORef a      -> LangL a
readVar      = undefined
writeVar     :: IORef a -> a -> LangL ()
writeVar     = undefined

-- Upper layer of business logic, the AppL eDSL

data AppF next where
  EvalLang    :: LangL a  -> (a -> next)  -> AppF next
  ForkProcess :: LangL () -> (() -> next) -> AppF next

instance Functor AppF where
  fmap f (EvalLang    act next) = EvalLang    act (f . next)
  fmap f (ForkProcess act next) = ForkProcess act (f . next)

type AppL = Free AppF
