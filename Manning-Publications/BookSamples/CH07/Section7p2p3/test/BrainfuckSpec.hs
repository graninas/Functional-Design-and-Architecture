module BrainfuckSpec where

import Test.Hspec

import qualified Data.Map as Map
import Data.IORef


incPointer, decPointer :: Char
jumpForward, jumpBackward :: Char
incVal, decVal :: Char
input :: Char
output :: Char


incPointer   = '>'
decPointer   = '<'
incVal       = '+'
decVal       = '-'
input        = ','
output       = '.'
jumpForward  = '['
jumpBackward = ']'


helloWorld :: String
helloWorld =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>" <>
  "+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.--" <>
  "----.--------.>>+.>++."

type Cell = Integer
type Val = Integer

data Runtime = Runtime
  { pointer :: IORef Cell
  , fields  :: IORef (Map.Map Cell (IORef Val))
  }

incVal' :: Runtime -> IO ()
incVal' (Runtime dpRef fsRef) = do
  dp <- readIORef dpRef
  fs <- readIORef fsRef
  case Map.lookup dp fs of
    Nothing  -> do
      valRef <- newIORef 1
      writeIORef fsRef $ Map.insert dp valRef fs
    Just valRef -> modifyIORef' valRef (\x -> x + 1)

decVal' :: Runtime -> IO ()
decVal' (Runtime dpRef fsRef) = do
  dp <- readIORef dpRef
  fs <- readIORef fsRef
  case Map.lookup dp fs of
    Nothing  -> do
      valRef <- newIORef (-1)
      writeIORef fsRef $ Map.insert dp valRef fs
    Just valRef -> modifyIORef' valRef (\x -> x - 1)

incDP dpRef = modifyIORef' dpRef (\x -> x + 1)
decDP dpRef = modifyIORef' dpRef (\x -> x - 1)

interpret :: Runtime -> String -> IO ()
interpret _ [] = pure ()
interpret rt@(Runtime dpRef fsRef) (ch:chs)
  | ch == incPointer = incDP dpRef >> interpret rt chs
  | ch == decPointer = decDP dpRef >> interpret rt chs
  | ch == incVal     = (incVal' rt) >> interpret rt chs
  | ch == decVal     = (decVal' rt) >> interpret rt chs
  | otherwise = error "Not implemented"

spec :: Spec
spec =
  describe "Brainfuck tests" $ do

    xit "Hello world" $ do
      1 `shouldBe` 1
