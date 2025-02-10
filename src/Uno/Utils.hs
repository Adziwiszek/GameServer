module Uno.Utils ( module Uno.Utils ) where

import Control.Monad.Cont
import Control.Monad.Random
import Control.Concurrent.STM (atomically, tryReadTChan, TChan)
import System.IO

import Uno.Common.Types
import Uno.Types
import Uno.Defaults
import Ui.Types
import Ui.Utils
import Utils


drainTChan :: TChan a -> IO [a]
drainTChan chan = do
  mItem <- atomically $ tryReadTChan chan
  case mItem of
    Nothing   -> return []
    Just item -> (item :) <$> drainTChan chan

{-----------------------------------------------------------------------------
    Board
 -----------------------------------------------------------------------------}
getCurrentColor :: SBoard -> GColor
getCurrentColor sboard = case getCardColor $ discardedCard sboard of
  Red -> red
  Blue -> blue
  Green -> green
  Yellow -> yellow
  _ -> red

class GetBoardInfo a where
  getCurrentPlayer :: a -> Player

instance GetBoardInfo Board where
  getCurrentPlayer board = 
    case boardPlayers board of
      Players (left, []) -> getCurrentPlayer $ board 
          { boardPlayers=Players ([], reverse left) }
      Players (_, right) -> head right

instance GetBoardInfo Players where
  getCurrentPlayer (Players (left, [])) = getCurrentPlayer $ Players ([], reverse left)
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
  

getAllPlayersList :: Board -> [Player]
getAllPlayersList board = 
  let (Players (l, r)) = boardPlayers board 
  in reverse l ++ r


getPlayerWithID :: Board -> Int -> Player
getPlayerWithID board pid = 
  let (Players (l, r)) = boardPlayers board
  in findPlayer (l ++ r)

  where
    findPlayer [] = undefined
    findPlayer (p : ps) = if playerID p == pid
      then p
      else findPlayer ps


initBoard :: MonadIO m => Players -> m Board 
initBoard (Players (left, right)) = do
  let allPlayers = left ++ right 
  g <- getStdGen 
  let cards = evalRand (shuffle generateStartingDeck) g
  let (finalPlayers, rest) = foldl setupPlayers ([], cards) allPlayers
  return $ Board {boardPlayers    = Players ([], finalPlayers), 
                  discardPile     = [head rest],
                  drawPile        = tail rest,
                  direction       = DRight,
                  skipPlayers     = [],
                  skipTurns       = 0,
                  addToPlayer     = 0,
                  canDraw         = True,
                  canTransferSkip = True,
                  chosenColor     = getCardColor $ head rest
                 , startedGame    = False
                  } 

  where
    setupPlayers (acc, cardsLeft) player =
        let (myCards, cardsLeft') = takeOut startingDeckSize cardsLeft in 
        ( player {playerHand=myCards} : acc
        , cardsLeft'
        )


reshuffleDeck :: MonadIO m => Board -> m Board
reshuffleDeck board = do
  g <- getStdGen 
  let top = getTopCard board
  let rest = tail $ discardPile board ++ drawPile board
  let newDrawPile = evalRand (shuffle rest) g
  return $ board {discardPile  = [top],
                  drawPile     = newDrawPile}


currentPlayerWaits :: Board -> Bool
currentPlayerWaits board = 
  let currentID = playerID $ getCurrentPlayer board 
  in skipTurns board > 0 || member currentID (skipPlayers board)



{-----------------------------------------------------------------------------
    Cards
 -----------------------------------------------------------------------------}

debugPrintCard :: Card -> IO ()
debugPrintCard card = putStrLn $ "Card clicked = " ++ show card

getCardRole :: Card -> CardRole
getCardRole (Card (r, _)) = r

getCardColor :: Card -> CardColor
getCardColor (Card (_, c)) = c

getTopCard :: Board -> Card
getTopCard = head . discardPile


changeCardNum :: Card -> Int -> Card
changeCardNum (Card (Number x, col)) y = Card (Number (x + y), col)
changeCardNum _ _ = undefined


addToDiscardPile :: Monad m => Board -> Card -> m Board
addToDiscardPile b c = return $ b {discardPile = c : discardPile b}


cardMember :: Card -> [Card] -> Bool
cardMember _ [] = False
cardMember (Card (ChangeColor _, _)) (Card (ChangeColor _, _):_) = True
cardMember (Card (AddColorless _, _)) (Card (AddColorless _, _):_) = True
cardMember a (x:xs) = a == x || cardMember a xs

cardsOfSameRole :: [Card] -> Bool
cardsOfSameRole [] = False
cardsOfSameRole (x:xs) = check xs x
  where
    check [] _ = True
    check (y:ys) c = getCardRole y == getCardRole c && check ys y
