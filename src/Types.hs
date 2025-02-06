{-# LANGUAGE DeriveGeneric #-}

module Types (module Types) where 

import GHC.Generics (Generic)
import Control.Concurrent
import System.IO
import Data.Binary

import Uno.Common.Types

{------------------------------------------------------------------------------
    Message  
------------------------------------------------------------------------------}

data MessageContent
  = Text String
  | GameState SBoard
  | GameMove [Card]
  | StartingGameInfo [SPlayer]
  deriving (Generic, Show)

data MessageTarget 
  = All
  | Normal -- don't send ones message to themselves
  | Server -- don't send to players 
  | ToPlayer Int -- send to a specific player
  deriving (Generic, Show) 

data Message = Message 
  { messageTarget :: MessageTarget
  , content :: MessageContent
  , senderID :: Int
  } 
  deriving (Generic, Show)

instance Binary MessageTarget
instance Binary MessageContent
instance Binary Message 

