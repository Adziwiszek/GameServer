{-# LANGUAGE DeriveGeneric #-}

module Types (module Types) where 

import GHC.Generics (Generic)
import Control.Concurrent
import System.IO
import Data.Binary


{------------------------------------------------------------------------------
    Message  
------------------------------------------------------------------------------}

data MessageContent
  = Text String
  | GameState SBoard
  deriving (Generic)

data MessageTarget 
  = All
  | Normal -- don't send ones message to themselves
  | Server -- don't send to players 
  | ToPlayer Int -- send to a specific player
  deriving (Generic, Show) 

data Message = Message {
  messageTarget :: MessageTarget,
  content :: MessageContent,
  senderID :: Int
} deriving (Generic)

instance Binary MessageTarget
instance Binary MessageContent
instance Binary Message 

{------------------------------------------------------------------------------
    Uno  
------------------------------------------------------------------------------}

data CardColor
  = Red
  | Blue
  | Yellow
  | Green
  | Colorless
  | Null
  deriving (Eq, Show, Generic)
instance Binary CardColor
  
data CardRole
  = Number Int
  | Add Int
  | Skip
  | Switch
  | ChangeColor CardColor
  | AddColorless (Int, CardColor)
  | SelfDraw
  | EndTurn
  deriving (Eq, Show, Generic)
instance Binary CardRole

newtype Card = Card (CardRole, CardColor) deriving (Eq, Show, Generic)
instance Binary Card

data Direction = DLeft | DRight deriving (Show, Generic)
instance Binary Direction

newtype Score = Score Player deriving (Generic)
-- instance Binary Score

class Monad m => UnoGame m  where
  getPlayerMove :: Int -> Board -> m [Card]

{-class MonadState Board m => UnoState m where
  getCurrentPlayer_ :: m Player
  updateCurrentPlayer :: Player -> m ()-}

data Player = Player
  { playerID :: Int
  , playerName :: String
  , hand     :: [Card]
  , playerHandle :: Handle
  , playerChannel :: Chan Message
  } deriving (Generic)

{- TODO
 -
 - Add types for player and board that will be send to players (hidden cards)
 - add channel and handle to player
 - -}

-- zipper for players
newtype Players = Players ([Player], [Player]) deriving Generic
-- instance Binary Players

data Board = Board {
  boardPlayers     :: Players,
  discardPile      :: [Card],
  drawPile         :: [Card],
  direction        :: Direction,
  skipPlayers      :: [Int],
  addToPlayer      :: Int,
  skipTurns        :: Int,
  canDraw          :: Bool,
  canTransferSkip  :: Bool,
  chosenColor      :: CardColor
} deriving Generic
-- instance Binary Board

instance Show Board where
    show board = 
        let playerStr = 
                foldl 
                    (\acc (Player {playerID=pid, hand=cs}) -> 
                        acc ++ "\nID = " ++ show pid ++ ", hand = " ++ show cs)
                    ""
                    (let (Players (l, r)) = boardPlayers board in l ++ r)
        in playerStr ++ 
          "\nDraw pile = " ++ show (drawPile board) ++ 
          "\nDiscard pile = " ++ show (discardPile board) ++ 
          "\nDirection = " ++ show (direction board) ++
          "\nTop color = " ++ show (chosenColor board)

-- Types for showing players game state

data SPlayer = SPlayer
  { splayerName :: String
  , snumOfCards :: Int
  } deriving (Generic, Show)
instance Binary SPlayer

newtype SPlayers = SPlayers [SPlayer] deriving (Show, Generic)
instance Binary SPlayers

newtype SScore = SScore SPlayer deriving (Show, Generic)
instance Binary SScore

data SBoard = SBoard 
  { otherPlayers  :: SPlayers
  , discardedCard :: Card
  , sdirection    :: Direction
  , myHand        :: [Card]
  } deriving (Generic, Show)
instance Binary SBoard
