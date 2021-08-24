module Andromeda.Hardware.Hdl where


import Andromeda.Hardware.Common


type ComponentIndex = String

data ComponentDef
  = Sensor     ComponentPassport ComponentIndex Parameter
  | Controller ComponentPassport ComponentIndex

type Hdl = [ComponentDef]
