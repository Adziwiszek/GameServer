module Uno.Defaults where

import Uno.Common.Types
import Uno.Types


defaultSBoard :: SBoard
defaultSBoard = SBoard 
  0 
  "" 
  (SPlayers []) 
  (Card (Blank, Colorless))
  DLeft
  []
  (0, "")
  0

startingDeckSize :: Int 
startingDeckSize = 7

defaultCard :: Card
defaultCard = Card (Blank, Colorless)

generateStartingDeck :: [Card]
generateStartingDeck = helper 
  ++ [Card(ChangeColor Null, Colorless) | _ <- ([1..8] :: [Int])] 
  ++ [Card(AddColorless (4, Null), Colorless) | _ <- ([1..8] :: [Int])]
  where 
    helper = do
      action <- [Number i | i <- [0..2]] ++ [Add 2]
      color <- [Red, Blue, Yellow, Green] 
      return $ Card (action, color)


