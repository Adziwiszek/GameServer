{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE InstanceSigs #-}

module Uno where 

import GHC.Generics (Generic)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Cont
import Control.Monad.Random
import Control.Monad (when)
--import Message

data CardColor
  = Red
  | Blue
  | Yellow
  | Green
  | Null
  deriving (Eq, Show)
  
data CardRole
  = Number Int
  | Add Int
  | Skip
  | Switch
  | SelfDraw
  | SelfSkip
  deriving (Eq, Show)

newtype Card = Card (CardRole, CardColor) deriving (Eq, Show)

data Direction = DLeft | DRight deriving Show

newtype Score = Score Player deriving Show

class Monad m => UnoGame m  where
  getPlayerMove :: Int -> Board -> m [Card]

{-class MonadState Board m => UnoState m where
  getCurrentPlayer_ :: m Player
  updateCurrentPlayer :: Player -> m ()-}

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
  direction      :: Direction,
  skipPlayers    :: [Int],
  addToPlayer    :: Int,
  canDraw        :: Bool
}

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
          "\nDirection = " ++ show (direction board)

data GameMessage 
  = IllegalMove
  | SkippingTurn
  | AddingCards

startingDeckSize :: Int 
startingDeckSize = 3

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove e (x:xs) = if e == x then xs else x : remove e xs

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

getNextPlayer :: Board -> Player
getNextPlayer board = getCurrentPlayer $ boardPlayers $ nextPlayer board

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
  action <- [Number i | i <- [0..9]]  ++ [Add 2]
  color <- [Red, Blue{-, Yellow, Green-}] 
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
        let (myCards, cardsLeft') = takeOut startingDeckSize cardsLeft in 
        (Player {playerID=pid, hand=myCards ++ [Card (Add 2, Red), Card (Add 2, Blue)]} : acc, cardsLeft')) 
        ([], cards) allPlayers
  return $ Board {boardPlayers  = Players ([], finalPlayers), 
                  discardPile   = [head rest],
                  drawPile      = tail rest,
                  direction     = DRight,
                  skipPlayers   = [],
                  addToPlayer   = 0,
                  canDraw       = True} 


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
      
removeCardsFromPlayer :: Board -> [Card] -> Board 
removeCardsFromPlayer board move = 
    let (Players (left, p:right)) = boardPlayers board 
    in let (Player {playerID=pid, hand=cards}) = p
    in let newHand = foldl 
            (\acc c -> 
                if member c move 
                then acc
                else c : acc) 
            [] cards
    in board {boardPlayers=Players (left, Player {playerID=pid, hand=newHand}:right)}

addToDiscardPile :: Monad m => Board -> Card -> m Board
addToDiscardPile b c = return $ b {discardPile = c : discardPile b}


canPlaceCard :: Card -> Card -> Bool
canPlaceCard (Card (r1, c1)) (Card (r2, c2)) = c1 == c2 || r1 == r2 


executeCardEffect :: MonadIO m => Board -> Card -> m Board
-- executeCardEffect b (Card (Add n, _)) = addToCurrentPlayer b n
executeCardEffect b (Card (Add n, col)) = return $ b {addToPlayer=addToPlayer b + n}
executeCardEffect b (Card (Number n, col)) = 
  return b 
--executeCardEffect b (Card (Skip, _)) = return b -- add a list to keep track of skipped players
{- executeCardEffect b (Card (Switch, _)) = 
  return $ b {direction= 
  case direction b of
    DLeft  -> DRight
    DRight -> DLeft
  }
-}
        
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
      -- check if player before added cards, current player can now 
      -- take those cards or play add card to give to the next player
      -- we check if a move is legal
      let currentPlayer = getCurrentPlayer $ boardPlayers board
      -- check if any effects are affecting this player
      let pid = playerID currentPlayer
      move <- getPlayerMove pid board
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
    processPlayerMove [Card (SelfDraw, _)] board =
      if canDraw board 
        then do
          let board' = board {canDraw = False}
          let cardsToDraw = addToPlayer board'
          if cardsToDraw > 0 
            then do
              b' <- addToCurrentPlayer board' cardsToDraw
              return $ Just $ b' { addToPlayer = 0 }
            else do
              b' <- addToCurrentPlayer board' 1
              return $ Just b'
        else return Nothing -- Player can't draw cards anymore
    processPlayerMove [Card (SelfSkip, _)] board = do
      let pid = playerID $ getCurrentPlayer $ boardPlayers board
      let newSkip = 
            if member pid $ skipPlayers board
              then remove pid $ skipPlayers board
              else skipPlayers board
      return $ Just $ nextPlayer $ board {skipPlayers = newSkip, canDraw=True}
    processPlayerMove move board = do      
      let hasCards = 
            all 
              (\c -> 
                let Player {playerID=_, hand=cs} = getCurrentPlayer $ boardPlayers board 
                in member c cs) 
              move
      if hasCards 
        then helper move $ nextPlayer $ removeCardsFromPlayer board move
        else return Nothing -- player doesn't have this card in their hand

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
        else return Nothing -- this is not a legal move
     
newtype TerminalUno x = TerminalUno { runTerminalUno :: IO x }

instance Functor TerminalUno where 
    fmap f (TerminalUno uno) = TerminalUno $ fmap f uno

instance Applicative TerminalUno where
    pure x = TerminalUno $ pure x
    TerminalUno f <*> TerminalUno x = TerminalUno $ f <*> x

instance Monad TerminalUno where 
    TerminalUno x >>= f = TerminalUno $ do
        result <- x
        runTerminalUno $ f result 

instance MonadIO TerminalUno where
    liftIO = TerminalUno

parseMap :: Map String Card
parseMap =  Map.insert "draw" (Card (SelfDraw, Null)) $
            Map.insert "skip" (Card (SelfSkip, Null)) $ 
            foldl (\acc x -> 
    case x of 
        Card (Number n, c) -> Map.insert (show n ++ show c) x acc
        Card (Add n, c)    -> Map.insert ("add" ++ show n ++ show c) x acc
    ) Map.empty generateStartingDeck
    
instance UnoGame TerminalUno where  
  getPlayerMove pid b = TerminalUno $ do
    putStrLn $ "Board :\n" ++ show b
    putStrLn $ "put your move player " ++ show pid
    let toDraw = addToPlayer b
    when (toDraw > 0) $ putStrLn $ "you have " ++ show toDraw ++ " cards to draw"
    line <- getLine
    let cards = parseCards line
    putStrLn $ "your move = " ++ show cards
    return cards 

    where
        parseCards strCards = 
            let cs = words strCards
            in foldl
                (\acc x -> case Map.lookup x parseMap of
                    Just c -> c : acc
                    Nothing -> acc)
                []
                cs

createPlayer :: Int -> Player
createPlayer pid = Player {playerID=pid, hand=[]}

runGame :: IO ()
runGame  = do
    let players = Players ([], [createPlayer 0, createPlayer 1])
    let game' :: TerminalUno (Score, Board)
        game' = game players
    (result, finalBoard) <- runTerminalUno game'
    putStrLn $ "final board: " ++ show finalBoard
    putStrLn $ "Score = " ++ show result
    
