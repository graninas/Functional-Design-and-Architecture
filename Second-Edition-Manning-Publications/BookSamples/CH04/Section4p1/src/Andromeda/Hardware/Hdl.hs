module Andromeda.Hardware.Hdl where


import Andromeda.Hardware.Common


type ComponentIndex = String

data ComponentDef = ComponentDef ComponentIndex ComponentPassport

type Hdl = [ComponentDef]
