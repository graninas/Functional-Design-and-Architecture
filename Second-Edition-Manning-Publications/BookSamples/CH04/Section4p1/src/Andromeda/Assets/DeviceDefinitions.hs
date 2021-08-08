module Andromeda.Assets.DeviceDefinitions where

import Andromeda.Hardware
import Andromeda.Assets.Vendors.AAA.ComponentDefinitions


-- Why don't we use VendorComponent here?
-- Because Hdl should be serializable and independent
-- of any implementation details.
-- It can use definitions only.


boostersDef :: Hdl
boostersDef =
  [ ComponentDef "nozzle1-t"  aaaTemperature25Passport
  , ComponentDef "nozzle1-p"  aaaPressure02Passport
  , ComponentDef "nozzle2-t"  aaaTemperature25Passport
  , ComponentDef "nozzle2-p"  aaaPressure02Passport
  , ComponentDef "controller" aaaController86Passport
  ]
