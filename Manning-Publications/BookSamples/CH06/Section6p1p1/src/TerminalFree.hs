module TerminalFree where

import Control.Monad.Free


data TerminalADT next
  = ReadLine (String -> next)
  | PrintLine String (() -> next)

instance Functor TerminalADT where
  fmap f (ReadLine next) = ReadLine (f . next)
  fmap f (PrintLine s next) = PrintLine s (f . next)

type Terminal a = Free TerminalADT a

readLine :: Terminal String
readLine = Free (ReadLine Pure)

printLine :: String -> Terminal ()
printLine s = Free (PrintLine s Pure)

terminal :: Terminal ()
terminal = do
  line1 <- readLine
  printLine line1

  line2 <- readLine
  printLine line2

