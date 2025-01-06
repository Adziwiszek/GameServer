{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (runGame, Board) where 

import System.IO
import Network.Socket
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Concurrent
import Message

class Monad m => TwoPlayerGame m s a b | m -> s a b where
  moveA :: s -> m a
  moveB :: s -> m b

class Monad m => TurnBasedGame m s x | m -> s x where
  -- takes state and playerID
  takePlayersMove :: s -> Int -> m x

data Score = AWins | Draw | BWins deriving (Eq, Ord)
instance Show Score where
  show AWins = "A wygrywa!"
  show BWins = "B wygrywa!"
  show Draw = "Remis!"

data Player = PlayerA | PlayerB deriving Eq
instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"

newtype Board = Board [Maybe Player]
instance Show Board where
  show (Board board) = 
    foldl (\acc (i, x) -> acc ++ " " ++
      maybe (show i) show x ++
      (if i `mod` 3 == 0 || i `mod` 3 == 1 
        then " |" 
        else 
        if i < 8 then " \n-----------\n"
        else "\n")) -- end of line 
    "\n\n" $ zip [0..] board

type AMove = Int
type BMove = Int

emptyBoard :: Board
emptyBoard = Board [Nothing | _ <- [0..8]]

(!?) :: [a] -> Int -> Maybe a 
(!?) xs i 
  | i < 0 || i > length xs = Nothing
  | otherwise = Just $ xs !! i

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

isValidMove :: Board -> Int -> Bool
isValidMove (Board b) move = 
  case b !? move of
    Just Nothing -> True
    Just _ -> False
    Nothing -> False

makeMove :: Board -> Int -> Player -> Maybe Board
makeMove (Board b) i player = 
  if isValidMove (Board b) i 
  then Just . Board $ replaceNth i (Just player) b
  else Nothing

checkWin :: Board -> Player -> Bool
checkWin board p = 
  any (\i -> checkRowWin board i p) [0..2] ||
  any (\i -> checkColWin board i p) [0..2] ||
  checkCrossWin board p 
    where
  checkRowWin (Board b) row p = 
    all (== Just p) [b !! (i + 3 * row) | i <- [0..2]]
  checkColWin (Board b) col p = 
    all (== Just p) [b !! (i * 3 + col) | i <- [0..2]]
  checkCrossWin (Board b) p = 
    all (== Just p) [b !! (i * 4) | i <- [0..2]] || 
    all (== Just p) [b !! (2 + 2 * i) | i <- [0..2]]

isFull :: Board -> Bool
isFull (Board b) = Nothing `notElem` b

isDraw :: Board -> Bool 
isDraw board = 
  isFull board &&
  not (checkWin board PlayerA) &&
  not (checkWin board PlayerB) 

checkScore :: Board -> Maybe Score
checkScore board =
  if checkWin board PlayerA then Just AWins
  else if checkWin board PlayerB then Just BWins
  else if isDraw board then Just Draw
  else Nothing

switchPlayer :: Player -> Player
switchPlayer PlayerA = PlayerB
switchPlayer PlayerB = PlayerA

makeLose :: Player -> Score
makeLose PlayerA = BWins
makeLose PlayerB = AWins

game :: TwoPlayerGame m Board AMove BMove => m (Score, Board)
game = play emptyBoard PlayerA 
  where 
    play board currentPlayer = do
      move <- case currentPlayer of { PlayerA -> moveA board; PlayerB -> moveB board }

      case makeMove board move currentPlayer of
        Nothing -> return (makeLose currentPlayer, board)
        Just newBoard -> 
          case checkScore newBoard of
            Nothing -> play newBoard (switchPlayer currentPlayer)
            Just s -> return (s, newBoard)

{-newtype IOGame s a b x = IOGame { runIOGame :: IO x }
  deriving (Functor, Applicative, Monad)

instance (Show s, Read a, Read b) => TwoPlayerGame (IOGame s a b) s a b where
  moveA board = IOGame $ do
    putStrLn $ "Aktualny stan planszy: " ++ show board
    putStrLn "Podaj ruch gracza A (liczba oznaczająca pole):"
    -- it should check here if move is valid 
    readLn

  moveB board = IOGame $ do
    putStrLn $ "Aktualny stan planszy: " ++ show board
    putStrLn "Podaj ruch gracza B (liczba oznaczająca pole):"
    readLn -}

type Players = [(Int, Handle)]
type InChan = Chan Message
type OutChan = Chan Message
newtype NetworkGame s a b x = NetworkGame { runNetworkGame :: InChan -> OutChan -> Players -> IO x }

instance Functor (NetworkGame s a b) where
  fmap f (NetworkGame io) = NetworkGame $ \inchan outchan players -> fmap f (io inchan outchan players)

instance Applicative (NetworkGame s a b) where
  pure x = NetworkGame $ \_ _ _ -> pure x
  NetworkGame f <*> NetworkGame x = NetworkGame $ \inchan outchan players -> f inchan outchan players <*> x inchan outchan players

instance Monad (NetworkGame s a b) where
  NetworkGame x >>= f = NetworkGame $ \inchan outchan players -> do
    result <- x inchan outchan players
    runNetworkGame (f result) inchan outchan players

movePlayer board player = NetworkGame $ \inchan outchan _ -> do
    let pID = case player of
          PlayerA -> 0
          PlayerB -> 1
    _broadcast outchan ("Turn of " ++ show player ++" Current board: " ++ show board) GameState All (-1)

    putStrLn $ "current player has id = " ++ show pID

    msg <- fix $ \loop -> do
      testMsg <- readChan inchan 
      
      putStrLn $ "testing input... " ++ show (content testMsg)

      if senderID testMsg == pID then
        return testMsg
      else do
        _broadcast outchan "Move from wrong player!" Text All 0
        loop
    return (read (content msg))

instance (Show s, Read a, Read b) => TwoPlayerGame (NetworkGame s a b) s a b where
  moveA board = movePlayer board PlayerA
  moveB board = movePlayer board PlayerB

runGame :: InChan -> OutChan -> Players -> IO ()
runGame inchan outchan players = do
  let game' :: NetworkGame Board AMove BMove (Score, Board)
      game' = game
  (result, finalBoard) <- runNetworkGame game' inchan outchan players
  _broadcast outchan ("Current board: " ++ show finalBoard) GameState All 0
  _broadcast outchan ("Wynik gry: " ++ show result) Text All 0
