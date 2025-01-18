{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Uno ( ) where 

import GHC.Generics (Generic)
import Data.List (find)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Random
import Control.Monad.State
--import Message

data CardColor
  = Red
  | Blue
  | Yellow
  | Green
  deriving (Eq, Show)
  
data CardRole
  = Number Int
  | Add Int
  | Skip
  | Switch
  deriving (Eq, Show)

newtype Card = Card (CardRole, CardColor) deriving (Eq, Show)

data Direction = DLeft | DRight

newtype Score = Score Player

class Monad m => UnoGame m  where
  getPlayerMove :: Int -> m [Card]

class MonadState Board m => UnoState m where
  getCurrentPlayer_ :: m Player
  updateCurrentPlayer :: Player -> m ()

data Player = Player
  { playerID :: Int
  , hand     :: [Card]
  } deriving (Generic, Show)

-- zipper for players
newtype Players = Players ([Player], [Player])

data Board = Board {
  boardPlayers   :: Players,
  discardPile    :: [Card],
  drawPile       :: [Card],
  direction      :: Direction
}

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member a (x:xs) = a == x || member a xs

takeOut :: Int -> [a] -> ([a], [a])
takeOut n xs = (take n xs, drop (n + 1) xs)

getTopCard :: Board -> Card
getTopCard = head . discardPile

getCurrentPlayer :: Players -> Player
getCurrentPlayer (Players (left, []))    = getCurrentPlayer (Players ([], reverse left))
getCurrentPlayer (Players (_, right)) = head right

nextPlayer :: Board -> Board
nextPlayer board = case direction board of 
  DRight -> playerRight board
  DLeft  -> playerLeft board

  where 
    playerRight b = case boardPlayers b of 
      Players (left, []) -> b {boardPlayers = Players ([], reverse left)}
      Players (left, [c]) -> b {boardPlayers = Players ([], reverse $ c : left)}
      Players (left, c : right) -> b {boardPlayers = Players (c : left, right)}

    playerLeft b = case boardPlayers b of 
      Players ([], right) -> b {boardPlayers = Players (tail $ reverse right, [last right])}
      Players (l : left, right) -> b {boardPlayers = Players (left, l : right)}
  

generateStartingDeck :: [Card]
generateStartingDeck = do
  action <- [Number i | i <- [0..9]] ++ [Add 2, Skip, Switch]
  color <- [Red, Blue, Yellow, Green] 
  return $ Card (action, color)


shuffle :: (RandomGen g) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  n <-  getRandomR (0, length xs - 1)
  let (nth, xs') = extractNth xs n
  rest <- shuffle xs'
  return $ nth : rest 
  where
    extractNth xs n = (xs !! n, take n xs ++ drop (n + 1) xs)


initBoard :: MonadIO m => Players -> m Board 
initBoard (Players (left, right)) = do
  let allPlayers = left ++ right 
  g <- getStdGen 
  let cards = evalRand (shuffle generateStartingDeck) g
  let (finalPlayers, rest) = foldl (\(acc, cardsLeft) (Player {playerID=pid, hand=_}) -> 
        let (myCards, cardsLeft') = takeOut 7 cardsLeft in 
        (Player {playerID=pid, hand=myCards} : acc, cardsLeft')) 
        ([], cards) allPlayers
  return $ Board {boardPlayers  = Players ([], finalPlayers), 
                  discardPile   = tail rest,
                  drawPile      = [head rest],
                  direction     = DRight} 


reshuffleDeck :: MonadIO m => Board -> m Board
reshuffleDeck board = do
  g <- getStdGen 
  let top = getTopCard board
  let rest = tail $ discardPile board ++ drawPile board
  let newDrawPile = evalRand (shuffle rest) g
  return $ board {discardPile  = [top],
                  drawPile     = newDrawPile}


addToCurrentPlayer :: MonadIO m => Board -> Int -> m Board
addToCurrentPlayer _board n = do
  board <- if length (drawPile _board) <= n 
            then reshuffleDeck _board 
            else return _board

  let (cards, draw) = splitAt n $ drawPile board

  case boardPlayers board of
    Players (left, c : right) -> do
      let newPlayers = Players (left, giveCardsToPlayer c cards : right)
      return $ board {boardPlayers = newPlayers, drawPile = draw}
    _ -> error "Impossible."
  where
    giveCardsToPlayer player cards = player {hand = cards ++ hand player}
      
      

addToDiscardPile :: Monad m => Board -> Card -> m Board
addToDiscardPile b c = return $ b {discardPile = c : discardPile b}


canPlaceCard :: Card -> Card -> Bool
canPlaceCard (Card (r1, c1)) (Card (r2, c2)) = c1 == c2 || r1 == r2 


executeCardEffect :: MonadIO m => Board -> Card -> m Board
executeCardEffect b (Card (Add n, _)) = addToCurrentPlayer b n
executeCardEffect b (Card (Number n, col)) = 
  return $ b {discardPile = Card (Number n, col) : discardPile b}
executeCardEffect b (Card (Skip, _)) = return b -- add a list to keep track of skipped players
executeCardEffect b (Card (Switch, _)) = 
  return $ b {direction= 
  case direction b of
    DLeft  -> DRight
    DRight -> DLeft
  }
        
{- Rules
 - player makes a move (gives list of cards they want to play)
 - check if they gave correct cards
 - go through each card
 - if player can place that card then do some action related to it
 - else return Nothing, player must try again
 -
 - Actions
 - when a card is played we do something
 - Add -> take x cards from the top of draw pile and add them to next players cards
 - Skip -> have next player skip their next turn. Multiple of those can stack
 -  so keep some list of ids that need to wait? If it is some players turn and their
 -  id is on skip list we remove it and skip that player
 - Switch -> switch direction of playing
 - Add to yourself -> when a player chooses to take a card they can play it immediately 
 - -}
        

-- uno logic
game :: (MonadIO m, UnoGame m) => Players -> m (Score, Board)
game players = do
  startingBoard <- initBoard players
  play startingBoard
  where         
    play :: (MonadIO m, UnoGame m) => Board -> m (Score, Board)
    play board = do
      -- move is a list of cards that the player played
      -- check if player chose to pick a card, they can then play it or pass
      -- we check if a move is legal
      let currentPlayer = getCurrentPlayer $ boardPlayers board
      let pid = playerID currentPlayer
      move <- getPlayerMove pid
      newBoard <- processPlayerMove move board

      case newBoard of 
        Nothing -> play board -- there was an illegal move, we try again
        Just b  -> evaluateBoardScore b 

    evaluateBoardScore b = do
      case lookForWinner b of
        Nothing -> play b
        Just s  -> return (Score s, b)

    -- checks if any player has 0 cards
    lookForWinner :: Board -> Maybe Player
    lookForWinner b = 
        find 
            (\pl -> 
                let Player {playerID=_, hand=cs} = pl 
                in length cs == 0) 
            (let Players (l, r) = boardPlayers b in l ++ r)  
       
    -- checks whether player actually has those cards and then 
    -- places them one by one and executes their effects
    processPlayerMove :: MonadIO m => [Card] -> Board -> m (Maybe Board)
    processPlayerMove move board = do      
      let hasCards = 
            all 
              (\c -> 
                let Player {playerID=_, hand=cs} = getCurrentPlayer $ boardPlayers board 
                in member c cs) 
              move
      if hasCards 
        then helper move board 
        else return Nothing

    -- goes through cards, checks if player can place them, and if so
    -- executes their effects
    helper :: MonadIO m => [Card] -> Board -> m (Maybe Board)
    helper [] board = return $ Just board
    helper (c : cs) board = 
      let top = getTopCard board in
      if c `canPlaceCard` top 
        then do
          b' <- addToDiscardPile board c
          b'' <- executeCardEffect b' c
          helper cs b''
        else return Nothing
     
      
