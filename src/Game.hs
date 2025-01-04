{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (runGame) where 

import Control.Monad
import Control.Concurrent
import Message

class Monad m => TwoPlayerGame m s a b | m -> s a b where
  moveA :: s -> m a
  moveB :: s -> m b

data Score = AWins | Draw | BWins deriving (Eq, Ord)
instance Show Score where
  show AWins = "A wygrywa!"
  show BWins = "B wygrywa!"
  show Draw = "Remis!"

data Symbol = A | B | Empty

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

game :: TwoPlayerGame m Board AMove BMove => m Score
game = play emptyBoard PlayerA 
  where 
    play board currentPlayer = do
      move <- case currentPlayer of { PlayerA -> moveA board; PlayerB -> moveB board }

      case makeMove board move currentPlayer of
        Nothing -> return $ makeLose currentPlayer -- move is illegal
        Just newBoard -> 
          case checkScore newBoard of
            Nothing -> play newBoard (switchPlayer currentPlayer)
            Just s -> return s

data GameState 
  = Lobby
  | Playing

newtype IOGame s a b x = IOGame { runIOGame :: IO x }
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
    readLn 

newtype NetworkGame s a b x = NetworkGame { runNetworkGame :: Chan Message -> IO x }

instance Functor (NetworkGame s a b) where
  fmap f (NetworkGame io) = NetworkGame $ \chan -> fmap f (io chan)

instance Applicative (NetworkGame s a b) where
  pure x = NetworkGame $ \_ -> pure x
  NetworkGame f <*> NetworkGame x = NetworkGame $ \chan -> f chan <*> x chan

instance Monad (NetworkGame s a b) where
  NetworkGame x >>= f = NetworkGame $ \chan -> do
    result <- x chan 
    runNetworkGame (f result) chan

instance (Show s, Read a, Read b) => TwoPlayerGame (NetworkGame s a b) s a b where
  moveA board = NetworkGame $ \chan -> do
    _broadcast chan ("Player A's turn. Current board: " ++ show board) "msg" All 0
    putStrLn "Waiting for Player A's move..."
    msg <- readChan chan
    putStrLn "dupa"
    putStrLn $ "player A move: " ++ show (content msg)
    return (read (content msg))

  moveB board = NetworkGame $ \chan -> do
    _broadcast chan ("Player B's turn. Current board: " ++ show board) "msg" All 0
    putStrLn "Waiting for Player B's move..."
    msg <- readChan chan
    return (read (content msg))

runGame :: Chan Message -> IO ()
runGame chan = do
  let game' :: NetworkGame Board AMove BMove Score
      game' = game
  result <- runNetworkGame game' chan 
  putStrLn $ "Wynik gry: " ++ show result
