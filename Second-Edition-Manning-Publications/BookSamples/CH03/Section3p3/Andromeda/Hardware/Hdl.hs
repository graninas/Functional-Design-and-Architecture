module Andromeda.Hardware.Hdl where


import Andromeda.Hardware.Common


type ComponentIndex = String

data Component
  = Sensor     ComponentPassport ComponentIndex Parameter
  | Controller ComponentPassport ComponentIndex

type Hdl = [Component]
