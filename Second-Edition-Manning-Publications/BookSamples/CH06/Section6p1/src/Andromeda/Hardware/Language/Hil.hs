module Andromeda.Hardware.Language.Hil where


import Andromeda.Hardware.Common
import Andromeda.Hardware.Domain

import Control.Monad.Free (Free (..), liftF)


data HilMethod next
  = GetStatus Controller (Either String Status -> next)
  | ReadSensor Controller ComponentIndex (Either String Measurement -> next)

instance Functor HilMethod where
  fmap f (GetStatus controller next) = GetStatus controller (f . next)
  fmap f (ReadSensor controller idx next) = ReadSensor controller idx (f . next)

type Hil a = Free HilMethod a

getStatus :: Controller -> Hil (Either String Status)
getStatus controller = liftF $ GetStatus controller id

readSensor :: Controller -> ComponentIndex -> Hil (Either String Measurement)
readSensor controller idx = liftF $ ReadSensor controller idx id
