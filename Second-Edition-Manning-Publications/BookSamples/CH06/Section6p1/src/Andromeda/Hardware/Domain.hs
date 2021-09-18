module Andromeda.Hardware.Domain where


type ComponentIndex = String
data Component = Component
data Controller = Controller ControllerName [Component]
