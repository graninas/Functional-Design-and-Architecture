module Listing1 where

data Device
  = Sensor
  | TerminalUnit

data NativeAPI
  = NativeAPI    -- to be defined

data Hdl
  = Devices [Device]
  | NativeAPIs [NativeAPI]
