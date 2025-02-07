{-# LANGUAGE DeriveGeneric #-}

module Uno.Common.Types where 

import GHC.Generics (Generic)
import Data.Binary (Binary)

{-----------------------------------------------------------------------------
    Card and Direction
    todo: explain why this split
 -----------------------------------------------------------------------------}
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
  | Blank
  | SelfDraw
  | EndTurn
  deriving (Eq, Show, Generic)
instance Binary CardRole

newtype Card = Card (CardRole, CardColor) deriving (Eq, Show, Generic)
instance Binary Card

data Direction = DLeft | DRight deriving (Show, Generic)
instance Binary Direction


{-----------------------------------------------------------------------------
    Types for stuff that will be sent to players. We don't want 
    them peeking on others cards;)
 -----------------------------------------------------------------------------}
 
data SPlayer = SPlayer
  { splayerName :: String
  , sid         :: Int
  , snumOfCards :: Int
  } deriving (Generic)
instance Binary SPlayer
instance Show SPlayer where
  show (SPlayer name pid cards) = "Player: " ++ show name ++ ", cards left: " ++ show cards 
      ++ ", id = " ++ show pid

newtype SPlayers = SPlayers [SPlayer] deriving (Show, Generic)
instance Binary SPlayers

newtype SScore = SScore SPlayer deriving (Show, Generic)
instance Binary SScore

data SBoard = SBoard 
  { myID          :: Int
  , myName        :: String
  , otherPlayers  :: SPlayers
  , discardedCard :: Card
  , sdirection    :: Direction
  , myHand        :: [Card]
  , currentPlayerInfo :: (Int, String)
  , cardsToDraw :: Int
  } deriving (Generic)
instance Binary SBoard

instance Show SBoard where
  show (SBoard _ name (SPlayers players) topcard dir hand (_, curName) ndraw) = flip joinStr " " $ 
    [("Current player = " ++ curName ++ "\n")] ++
    [(if ndraw > 0 then "they have to draw " ++ show ndraw ++ " cards\n" else "")] ++
    map showNewline players ++
    [ "Top card: " ++ show topcard ++ "\n"
    , "Direction: " ++ show dir ++ "\n"
    , "Your name: " ++ name ++ "\n"
    , "Your hand: \n" 
    ] ++ 
    map showNewline hand

    where 
    showNewline x = show x ++ "\n"

joinStr :: [String] -> String -> String
joinStr xs space = foldl (\acc word -> acc ++ space ++ word) "" xs
