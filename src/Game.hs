{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game (runGame, Board, movePlayer) where 

import System.IO
import Control.Monad.Fix (fix)
import Control.Concurrent
import Message

class Monad m => TwoPlayerGame m a b | m -> a b where
  moveA :: Board -> m a
  moveB :: Board -> m b

data Score = AWins | Draw | BWins deriving (Eq, Ord)
instance Show Score where
  show AWins = "A wygrywa!"
  show BWins = "B wygrywa!"
  show Draw = "Remis!"


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

game :: TwoPlayerGame m AMove BMove => m (Score, Board)
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

type InChan = Chan Message
type OutChan = Chan Message
type Players = [(Int, Chan Message, Handle)]
type Turn = MVar Int
newtype NetworkGame a b x = NetworkGame 
  { runNetworkGame :: OutChan -> Players -> Turn -> IO x 
  }

instance Functor (NetworkGame a b) where
  fmap f (NetworkGame io) = NetworkGame $ \outchan players turn -> fmap f (io outchan players turn)

instance Applicative (NetworkGame a b) where
  pure x = NetworkGame $ \_ _ _ -> pure x
  NetworkGame f <*> NetworkGame x = NetworkGame $ \outchan players turn 
    -> f outchan players turn <*> x outchan players turn

instance Monad (NetworkGame a b) where
  NetworkGame x >>= f = NetworkGame $ \outchan players turn -> do
    result <- x outchan players turn
    runNetworkGame (f result) outchan players turn

movePlayer board player = NetworkGame $ \outchan players turn -> do
    let pID = case player of
          PlayerA -> 0
          PlayerB -> 1
    modifyMVar_ turn $ \_ -> return pID
    let inchan = case lookup pID $ map (\(_pId, chan, _) -> (_pId, chan)) players of
          Nothing -> error $ "No channel for player with ID = " ++ show pID
          Just chan -> chan

    _broadcast outchan (Text $ "Turn of " ++ show player ++" Current board: " ++ show board)  All (-1)
    sendToPlayer outchan pID $ Text "Your turn"    

    msg <- fix $ \loop -> do
      testMsg <- readChan inchan 
      if senderID testMsg == pID then
        return testMsg
      else do
        _broadcast outchan (Text "Move from wrong player!") All 0
        loop
    return (read (unpackStringMessage msg "dupa"))

instance (Read a, Read b) => TwoPlayerGame (NetworkGame a b) a b where
  moveA board = movePlayer board PlayerA
  moveB board = movePlayer board PlayerB

runGame :: OutChan -> Players -> Turn -> IO ()
runGame outchan players turn = do
  let game' :: NetworkGame AMove BMove (Score, Board)
      game' = game
  (result, finalBoard) <- runNetworkGame game' outchan players turn
  _broadcast outchan (Text $ "Current board: " ++ show finalBoard) All 0
  _broadcast outchan (Text $ "Wynik gry: " ++ show result) All 0
