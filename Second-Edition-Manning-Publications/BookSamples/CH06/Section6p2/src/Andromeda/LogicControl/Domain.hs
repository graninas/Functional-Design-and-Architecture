module Andromeda.LogicControl.Domain where


type Message = String
type Key = String



data LogicFailure
  = LogicFailure String
  deriving (Show, Eq, Ord)
