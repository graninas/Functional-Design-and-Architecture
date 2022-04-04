module Andromeda.Common.Property where

import Andromeda.Common.Value
import Andromeda.Common.Physics

type PropertyName = String

data Property
  = ValueProperty PropertyName Value
  | PhysicalUnitProperty PropertyName PhysicalUnit
  deriving (Show, Eq, Ord)
