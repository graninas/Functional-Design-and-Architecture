{-# LANGUAGE GADTs #-}

module Lang where

import           Control.Monad
import           Control.Monad.Free     (Free, liftF)

type VarId = Int

-- | Concurrent variable (STM TVar).
newtype StateVar a = StateVar
  { _varId :: VarId
  }

-- | State language. It reflects STM and its behavior.
data StateF next where
  NewVar   :: a -> (StateVar a -> next) -> StateF next
  ReadVar  :: StateVar a -> (a -> next) -> StateF next
  WriteVar :: StateVar a -> a -> (() -> next) -> StateF next
  Retry    :: StateF next

instance Functor StateF where
  fmap f (NewVar   val     next) = NewVar   val     (f . next)
  fmap f (ReadVar  var     next) = ReadVar  var     (f . next)
  fmap f (WriteVar var val next) = WriteVar var val (f . next)
  fmap f Retry                   = Retry

type StateL = Free StateF

newVar :: a -> StateL (StateVar a)
newVar val = liftF $ NewVar val id

readVar :: StateVar a -> StateL a
readVar var = liftF $ ReadVar var id

writeVar :: StateVar a -> a -> StateL ()
writeVar var val = liftF $ WriteVar var val id

retry :: StateL a
retry = liftF Retry

-- | Lang language. Aggregates services useful for business logic.
data LangF next where
  Delay               :: Int        -> (() -> next)  -> LangF next
  GetRandomInt        :: (Int, Int) -> (Int -> next) -> LangF next
  EvalStateAtomically :: StateL a   -> (a -> next)   -> LangF next

instance Functor LangF where
  fmap f (Delay        t       next) = Delay        t       (f . next)
  fmap f (GetRandomInt range   next) = GetRandomInt range   (f . next)
  fmap f (EvalStateAtomically act next) = EvalStateAtomically act (f . next)

type LangL = Free LangF

atomically :: StateL a -> LangL a
atomically act = liftF $ EvalStateAtomically act id

delay :: Int -> LangL ()
delay t = liftF $ Delay t id

getRandomInt :: (Int, Int)   -> LangL Int
getRandomInt range = liftF $ GetRandomInt range id

-- | App langauge. Aggregates facilities useful for app construction.
data AppF next where
  EvalLang    :: LangL a  -> (a -> next)  -> AppF next
  ForkProcess :: LangL () -> (() -> next) -> AppF next

instance Functor AppF where
  fmap f (EvalLang    act next) = EvalLang    act (f . next)
  fmap f (ForkProcess act next) = ForkProcess act (f . next)

type AppL = Free AppF

forkProcess :: LangL () -> AppL ()
forkProcess act = liftF $ ForkProcess act id

evalLang :: LangL a  -> AppL a
evalLang act = liftF $ EvalLang act id
