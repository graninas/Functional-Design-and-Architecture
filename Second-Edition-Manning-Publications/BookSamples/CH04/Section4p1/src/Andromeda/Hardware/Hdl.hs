module Andromeda.Hardware.Hdl where


import Andromeda.Hardware.Common


type ComponentIndex = String

data Component
  = Sensor     ComponentDef ComponentIndex Parameter
  | Controller ComponentDef ComponentIndex

type Hdl = [Component]
