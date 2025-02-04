{-# LANGUAGE DeriveGeneric #-}

module Uno.Types where

import GHC.Generics (Generic)
import Control.Concurrent
import System.IO
import Data.Binary

import Types
import Uno.Common.Types

newtype Score = Score Player deriving (Generic)
-- instance Binary Score

class Monad m => UnoGame m  where
  getPlayerMove :: Int -> Board -> m [Card]

data Player = Player
  { playerID :: Int
  , playerName :: String
  , playerHand     :: [Card]
  , playerHandle :: Handle
  , playerChannel :: Chan Message
  } deriving (Generic)
instance Show Player where
  show (Player _ name _ _ _) = "Player: " ++ name ++ " "

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
                    (\acc (Player {playerID=pid, playerHand=cs}) -> 
                        acc ++ "\nID = " ++ show pid ++ ", hand = " ++ show cs)
                    ""
                    (let (Players (l, r)) = boardPlayers board in l ++ r)
        in playerStr ++ 
          "\nDraw pile = " ++ show (drawPile board) ++ 
          "\nDiscard pile = " ++ show (discardPile board) ++ 
          "\nDirection = " ++ show (direction board) ++
          "\nTop color = " ++ show (chosenColor board)

-- Types for showing players game state

