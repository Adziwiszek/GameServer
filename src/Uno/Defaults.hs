module Uno.Defaults where

import Uno.Common.Types
import Uno.Types


startingDeckSize :: Int 
startingDeckSize = 2


defaultCard :: Card
defaultCard = Card (Blank, Colorless)


generateStartingDeck :: [Card]
generateStartingDeck = helper ++ [Card(ChangeColor Null, Colorless) | _ <- ([1..2] :: [Int])] ++ [Card(AddColorless (4, Null), Colorless) | _ <- ([1..2] :: [Int])]
  where 
    helper = do
      action <- [Number i | i <- [0..9]] ++ [Add 2]
      color <- [Red, Blue, Yellow, Green] 
      return $ Card (action, color)


