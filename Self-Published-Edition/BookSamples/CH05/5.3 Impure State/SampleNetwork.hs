module SampleNetwork where

import Andromeda.Hardware
import Andromeda.Simulator

aaaManufacturer = "AAA Inc."
guid1 = "3539390d-f189-4434-bd9e-d39e494a869a"
guid2 = "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4"
guid3 = "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5"

aaa_p_02 = component sensors guid1 aaaManufacturer "Pressure sensor AAA-P-02"
aaa_t_25 = component sensors guid2 aaaManufacturer "Temperature sensor AAA-T-25"
aaa_controller_01 = component controllers guid3 aaaManufacturer "Controller AAA-C-01"

nozzle1TCompIdx, nozzle2TCompIdx, nozzle1PCompIdx, nozzle2PCompIdx :: ComponentIndex
boostersControllerCompIdx :: ComponentIndex
nozzle1TCompIdx = "nozzle1-t"
nozzle2TCompIdx = "nozzle2-t"
nozzle1PCompIdx = "nozzle1-p"
nozzle2PCompIdx = "nozzle2-P"
boostersControllerCompIdx = "boosters-controller"

boostersAddr :: PhysicalAddress
boostersAddr = "01"

boostersDef :: Hdl ()
boostersDef = do
    sensor aaa_t_25 nozzle1TCompIdx temperature
    sensor aaa_t_25 nozzle2TCompIdx temperature
    sensor aaa_p_02 nozzle1PCompIdx pressure
    sensor aaa_p_02 nozzle2PCompIdx pressure
    controller aaa_controller_01 boostersControllerCompIdx

{-
    [2 temperature, 2 pressure]
        :
    Boosters
        :
01  Boosters controller
02      |
03  Boosters RTU
04      |
05      |
06      |
07      |
08      |
09  Logic Control
-}

-- terminal units are computers inside network
-- controllers are computers inside device

networkDef :: Hndl ()
networkDef = do
    iBoosters <- remoteDevice boostersAddr boostersDef
    iBoostersTU <- terminalUnit "03"
    linkedDevice iBoosters iBoostersTU    
    iLogicControl <- logicControl "09"
    link iLogicControl [iBoostersTU]

boostersNozzle1T   = (boostersAddr, nozzle1TCompIdx)
boostersNozzle1P   = (boostersAddr, nozzle1PCompIdx)
boostersNozzle2T   = (boostersAddr, nozzle2TCompIdx)
boostersNozzle2P   = (boostersAddr, nozzle2PCompIdx)
boostersController = (boostersAddr, boostersControllerCompIdx)



