module StateTMonadSpec where

import Test.Hspec

import Control.Monad.State (StateT, get, put, runStateT, execStateT, lift)
import qualified Data.Map as Map


type X = Int
type Y = Int

data CellType
  = Bomb
  | Num Int
  | Empty
  deriving (Show, Eq, Ord)

data Cell
  = Hidden CellType
  | Revealed CellType
  deriving (Show, Eq, Ord)

type Board = Map.Map (X, Y) Cell

type BoardMonad = StateT Board IO


revealCell :: (X, Y) -> BoardMonad ()
revealCell (x, y) = do
  board <- get

  lift (putStrLn ("Trying cell " <> show (x, y)))

  case Map.lookup (x, y) board of
    Nothing -> lift (putStrLn "No such cell.")
    (Just (Revealed _)) -> lift (putStrLn "Already revealed.")
    (Just (Hidden Bomb)) -> lift (putStrLn "It’s bomb! You loose!")
    (Just (Hidden c)) -> do
      let newBoard = Map.insert (x, y) (Revealed c) board
      put newBoard
      lift (putStrLn "Cell revealed.")


type Score = Int
type MinesweeperGame a = StateT Score BoardMonad a

printGame :: MinesweeperGame ()
printGame = do
  score <- get                      -- no lifting
  board <- lift get                 -- lift once

  lift (lift (putStrLn (            -- lift twice
    "Score: " <> show score
    <> ", board: " <> show board
    )))



board1 :: Board
board1 = Map.fromList
  [ ( (1, 1), Hidden Empty )
  , ( (2, 2), Hidden Bomb  )
  ]

board2 :: Board
board2 = Map.fromList
  [ ( (1, 1), Revealed Empty )
  , ( (2, 2), Hidden Bomb  )
  ]

spec :: Spec
spec =
  describe "StateT monad tests" $ do
    it "BoardMonad stack" $ do
      (_, board') <- runStateT (revealCell (1, 1)) board1

      board' `shouldBe` board2

    it "MinesweeperGame stack" $ do
      let runGame :: BoardMonad Score = execStateT printGame 0
      let runBoard :: IO Board = execStateT runGame board1

      board' <- runBoard
      board' `shouldBe` board1
