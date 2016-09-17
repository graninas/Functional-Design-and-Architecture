{-# LANGUAGE DeriveFunctor #-}
module Andromeda.Hardware.HNDL where

import Control.Monad.State.Class
import Control.Monad.Free
import qualified Data.Map as M

import Andromeda.Hardware.HDL

type PhysicalAddress = String

data DeviceInterface = DeviceInterface PhysicalAddress
  deriving (Show, Eq)
data TerminalUnitInterface = TerminalUnitInterface PhysicalAddress
  deriving (Show, Eq)
data LogicControlInterface = LogicControlInterface PhysicalAddress
  deriving (Show, Eq)

-- | Convenient language for defining devices in network.
data NetworkComponent a
    = DeviceDef PhysicalAddress (Hdl ()) (DeviceInterface -> a)
    | TerminalUnitDef PhysicalAddress (TerminalUnitInterface -> a)
    | LogicControlDef PhysicalAddress (LogicControlInterface -> a)
    | LinkedDeviceDef DeviceInterface TerminalUnitInterface a
    | LinkDef LogicControlInterface [TerminalUnitInterface] a
  deriving (Functor)

-- | Free monad Hardware Network Definition Language.
-- It's just a definition, but not a real hardware network.
type Hndl a = Free NetworkComponent a

class HndlInterpreter m where
   onDeviceDef :: Monad m => PhysicalAddress -> Hdl () -> m DeviceInterface
   onTerminalUnitDef :: Monad m => PhysicalAddress -> m TerminalUnitInterface
   onLogicControlDef :: Monad m => PhysicalAddress -> m LogicControlInterface
   onLinkedDeviceDef :: Monad m => DeviceInterface -> TerminalUnitInterface -> m ()
   onLinkDef         :: Monad m => LogicControlInterface -> [TerminalUnitInterface] -> m ()
   
mkLogicControlInterface = LogicControlInterface
mkDeviceInterface = DeviceInterface
mkTerminalUnitInterface = TerminalUnitInterface

interpretHndl :: (Monad m, HndlInterpreter m) => Hndl a -> m a
interpretHndl (Pure a) = return a
interpretHndl (Free proc) = case proc of
    DeviceDef pa hdl next -> do
        i <- onDeviceDef pa hdl
        interpretHndl $ next i
    TerminalUnitDef pa next -> do
        i <- onTerminalUnitDef pa
        interpretHndl $ next i
    LogicControlDef pa next -> do
        i <- onLogicControlDef pa
        interpretHndl $ next i
    LinkedDeviceDef rdi tui next -> do
        onLinkedDeviceDef rdi tui
        interpretHndl $ next
    LinkDef i tuis next -> do
        onLinkDef i tuis
        interpretHndl $ next

remoteDevice :: PhysicalAddress -> Hdl () -> Hndl DeviceInterface
remoteDevice pa hdl = liftF $ DeviceDef pa hdl id

terminalUnit :: PhysicalAddress -> Hndl TerminalUnitInterface
terminalUnit pa = liftF $ TerminalUnitDef pa id

logicControl :: PhysicalAddress -> Hndl LogicControlInterface
logicControl pa = liftF $ LogicControlDef pa id

linkedDevice :: DeviceInterface -> TerminalUnitInterface -> Hndl ()
linkedDevice rdi tui = liftF $ LinkedDeviceDef rdi tui ()

link :: LogicControlInterface -> [TerminalUnitInterface] -> Hndl ()
link i tuis = liftF $ LinkDef i tuis ()
