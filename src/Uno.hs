{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Uno ( ) where 

import System.IO
import Network.Socket
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Concurrent
import Message

class Monad m => TurnBasedGame m s x | m -> s x where
  -- takes state and playerID
  takePlayersMove :: s -> Int -> m x

data CardColor
  = Red
  | Blue
  | Yellow
  | Green

data CardRole
  = Number Int
  | Add Int
  | Stop
  | Switch


