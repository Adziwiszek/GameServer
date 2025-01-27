module Uno (game, runGame, runNetworkGame, parseCards) where 

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Cont
import Control.Monad.Random
import System.IO
import Control.Monad.Fix (fix)
import Control.Concurrent
-- import Control.Monad (when)

import Types
import Message

{-
data GameMessage 
  = IllegalMove
  | NoSuchCard
-}

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

startingDeckSize :: Int 
startingDeckSize = 3

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove e xs = rm xs []
  where 
    rm [] _ = xs
    rm (x:xs') acc = if e == x then reverse acc ++ xs' else rm xs' (x:acc) 

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member a (x:xs) = a == x || member a xs

cardMember :: Card -> [Card] -> Bool
cardMember _ [] = False
cardMember (Card (ChangeColor _, _)) (Card (ChangeColor _, _):_) = True
cardMember (Card (AddColorless _, _)) (Card (AddColorless _, _):_) = True
cardMember a (x:xs) = a == x || cardMember a xs

takeOut :: Int -> [a] -> ([a], [a])
takeOut n xs = (take n xs, drop (n + 1) xs)

getCardRole :: Card -> CardRole
getCardRole (Card (r, _)) = r

getCardColor :: Card -> CardColor
getCardColor (Card (_, c)) = c

getTopCard :: Board -> Card
getTopCard = head . discardPile

getTopColor :: Board -> CardColor
getTopColor b = case getCardColor $ getTopCard b of
  Colorless -> chosenColor b
  c         -> c

cardsOfSameRole :: [Card] -> Bool
cardsOfSameRole [] = False
cardsOfSameRole (x:xs) = check xs x
  where
    check [] _ = True
    check (y:ys) c = getCardRole y == getCardRole c && check ys y

{-getCurrentPlayer :: Board -> Player
getCurrentPlayer board = 
  case boardPlayers board of
    Players (left, []) -> getCurrentPlayer $ board {boardPlayers=Players ([], reverse left)}
    Players (_, right) -> head right-}

getNextPlayer :: Board -> Player
getNextPlayer board = getCurrentPlayer $ nextPlayer board

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
generateStartingDeck = helper ++ {- [Card(ChangeColor Null, Colorless) | _ <- [1..4]] ++ -} [Card(AddColorless (4, Null), Colorless) | _ <- [1..10]]
  where 
    helper = do
      action <- [Number i | i <- [0..4]] -- ++ [Switch, Skip]
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
    extractNth xs' n = (xs' !! n, take n xs' ++ drop (n + 1) xs')


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
    giveCardsToPlayer player cards = player {playerHand = cards ++ playerHand player}
      
removeCardsFromPlayer :: Board -> [Card] -> Board 
removeCardsFromPlayer board move = 
    let (Players (left, p:right)) = boardPlayers board 
    in let (Player {playerID=pid, playerHand=cards}) = p
    in let newHand = foldl 
            (\acc c -> 
                if cardMember c move 
                then acc
                else c : acc) 
            [] cards
    in board {boardPlayers=Players (left, Player {playerID=pid, playerHand=newHand}:right)}

currentPlayerWaits :: Board -> Bool
currentPlayerWaits board = 
  let currentID = playerID $ getCurrentPlayer board 
  in skipTurns board > 0 || member currentID (skipPlayers board)

addToDiscardPile :: Monad m => Board -> Card -> m Board
addToDiscardPile b c = return $ b {discardPile = c : discardPile b}


canPlaceCard :: Card -> Board -> Bool
canPlaceCard (Card (r1, c1)) b = 
  let (Card (r2, _)) = getTopCard b 
  in r1 == r2 || c1 == Colorless || c1 == chosenColor b

executeCardEffect :: MonadIO m => Board -> Card -> m Board
executeCardEffect b (Card (ChangeColor c, _)) = return $ b {chosenColor=c}
executeCardEffect b (Card (AddColorless (n, c), _)) = return $ b {chosenColor = c, addToPlayer=addToPlayer b + n}
executeCardEffect b (Card (Add n, _))       = return $ b {addToPlayer=addToPlayer b + n}
executeCardEffect b (Card (Number _, c))    = return $ b {chosenColor=c}
executeCardEffect b (Card (Skip, _))        = return $ b {skipTurns=skipTurns b + 1}
executeCardEffect b (Card (Switch, _)) = 
  return $ b {direction= 
  case direction b of
    DLeft  -> DRight
    DRight -> DLeft
  }
-- self cards are not processed here
-- because they have Null color, they can't be placed on any other cards,
-- so we don't need to worry about it here
executeCardEffect b (Card (SelfDraw, _)) = return b 
executeCardEffect b (Card (EndTurn, _)) = return b 

processPlayerMove :: MonadIO m => [Card] -> Board -> m (Maybe Board)
processPlayerMove move board = case move of 
  [Card (SelfDraw, _)] -> processSelfDraw board
  [Card (EndTurn, _)]  -> processEndTurn board
  _                    -> processRegularMove move board
        

processSelfDraw :: MonadIO m => Board -> m (Maybe Board)
processSelfDraw board
  | not (canDraw board) && not (currentPlayerWaits board) = return Nothing
  | otherwise = do
      let board' = board {canDraw = False}
      let cardsToDraw = max 1 (addToPlayer board')
      drawnBoard <- addToCurrentPlayer board' cardsToDraw
      return $ Just $ drawnBoard {addToPlayer = 0}

processEndTurn :: MonadIO m => Board -> m (Maybe Board)
processEndTurn board 
  | canDraw board && not (currentPlayerWaits board) = return Nothing 
  | otherwise = do
    let currentID = playerID $ getCurrentPlayer board
    let newSkipList = if skipTurns board == 0 
        then remove currentID $ skipPlayers board
        else skipPlayers board ++ [currentID | _ <- [1..skipTurns board - 1]]
    return $ Just $ nextPlayer $ board
      { skipPlayers     = newSkipList
      , canDraw         = True
      , skipTurns       = 0
      , canTransferSkip = True
      }

processRegularMove :: MonadIO m => [Card] -> Board -> m (Maybe Board)
processRegularMove move board = do
  if not (hasCards move board) || not (cardsOfSameRole move)
    then return Nothing
    else do
      processedBoard <- processCards move $ removeCardsFromPlayer board move
      case processedBoard of
        Nothing -> return Nothing
        Just b  -> return $ Just $ nextPlayer b
  where 
    hasCards cards b = all (\c -> cardMember c (playerHand $ getCurrentPlayer b)) cards

processCards :: MonadIO m => [Card] -> Board -> m (Maybe Board)
processCards [] board = return $ Just $ board {canDraw = True}
processCards (c:cs) board 
  | not (canPlayCard c board) = return Nothing
  | otherwise = do
    newBoard <- addToDiscardPile board c
    effectBoard <- executeCardEffect newBoard c
    processCards cs effectBoard
  where 
    canPlayCard card b = 
      canPlaceCard card b &&
      (skipTurns b == 0 || (skipTurns b > 0 && getCardRole card == Skip))
        
    
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
      let currentPlayer = getCurrentPlayer board
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
                let Player {playerID=_, playerHand=cs} = pl 
                in length cs == 0) 
            (let Players (l, r) = boardPlayers b in l ++ r)  


boardToSBoard :: Board -> SBoard
boardToSBoard b = 
  let currentPlayer = getCurrentPlayer b in
  let pid = playerID currentPlayer in 
  let (Players (left, right)) = boardPlayers b in
  let otherPlayers_ :: SPlayers
      otherPlayers_ =  SPlayers $ map toSPlayer $ filter (filterOutCurrent pid) left ++ right  in
  let discardedCard_ = head $ discardPile b in 
  let myHand_ = playerHand currentPlayer in
  let sdirection_ = direction b in
  let myName = playerName currentPlayer in
  SBoard otherPlayers_ discardedCard_ sdirection_ myHand_  myName
       
  where 
    filterOutCurrent :: Int -> Player -> Bool
    filterOutCurrent pid (Player pid' _ _ _ _) = pid' /= pid

    toSPlayer (Player _ name hand _ _) = SPlayer 
      { splayerName = name
      , snumOfCards = length hand
      }

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

parseCardMap :: Map String Card
parseCardMap = 
    Map.insert "draw" (Card (SelfDraw, Null)) $
    Map.insert "endturn" (Card (EndTurn, Null)) $
    foldl (\acc x -> 
    case x of 
        Card (Number n, c)       -> Map.insert (show n ++ show c) x acc
        Card (Add n, c)          -> Map.insert ("add" ++ show n ++ show c) x acc
        Card (Skip, c)           -> Map.insert ("skip" ++ show c) (Card (Skip, c)) acc
        Card (Switch, c)         -> Map.insert ("switch" ++ show c) (Card (Switch, c)) acc
        Card (ChangeColor _, _)  -> Map.insert "toBlue" (Card (ChangeColor Blue, Colorless)) $ Map.insert "toRed" (Card (ChangeColor Red, Colorless)) acc
        Card (EndTurn, _)        -> Map.insert "endturn" (Card (EndTurn, Null)) acc
        Card (SelfDraw, _)       -> Map.insert "draw" (Card (SelfDraw, Null)) acc
        Card (AddColorless (n, _), _) -> Map.insert "waddRed" (Card (AddColorless (n, Red), Colorless)) acc
    ) Map.empty generateStartingDeck
    


parseCards :: String -> [Card]
parseCards strCards = 
    let cs = words strCards
    in foldl
        (\acc x -> case Map.lookup x parseCardMap of
            Just c -> c : acc
            Nothing -> acc)
        []
        cs

instance UnoGame TerminalUno where  
  getPlayerMove pid' b' = TerminalUno $ do
    getStrMove pid' b'

    where
        getStrMove pid b = do
          putStrLn $ "\nBoard :" ++ show b
          putStrLn $ "Can draw = " ++ show (canDraw b)
          putStrLn $ "put your move player " ++ show pid
          when (skipTurns b > 0) $ putStrLn $
              "you are facing " ++ show (skipTurns b) ++ " skipped turns"
          when (member pid $ skipPlayers b) $ putStrLn "you are skipping a turn"
          let toDraw = addToPlayer b
          when (toDraw > 0) $ putStrLn $ "you have " ++ show toDraw ++ " cards to draw"
          line <- getLine
          let cards = parseCards line
          -- unless (cardsOfSameRole cards) $ putStrLn "you must play cards of the same role"
          case cards of 
            {-[Card (EndTurn, _)] | canDraw b && skipTurns b == 0 -> do
              putStrLn "Draw a card first!!!"
              getStrMove pid b-}
            [] -> do
              putStrLn "You can't play empty hand!!!"
              getStrMove pid b
            _  -> do
              putStrLn $ "your move = " ++ show cards
              return cards 


createPlayer :: Int -> Handle -> Chan Message -> String -> Player
createPlayer pid han chan name = Player pid name [] han chan 

runGame :: OutChan -> [(Int, String, Chan Message, Handle)] ->  IO ()
runGame outchan serverPlayers = do
    let players = map (\(pid, name, chan, handle) -> createPlayer pid handle chan name) serverPlayers
    {-let players = Players ([], [createPlayer 0, createPlayer 1, createPlayer 2, createPlayer 3])
    let game' :: TerminalUno (Score, Board)
        game' = game players
    (result, finalBoard) <- runTerminalUno game'-}
    -- putStrLn $ "final board: " ++ show finalBoard
    -- putStrLn $ "Score = " ++ show result
    return ()

    
type InChan = Chan Message
type OutChan = Chan Message
-- type Players = [(Int, Chan Message, Handle)]
type Turn = MVar Int
newtype NetworkUno x = NetworkUno
  { runNetworkUno :: OutChan -> Players ->  IO x
  }

instance Functor NetworkUno where
  fmap f (NetworkUno io) = NetworkUno $ \outchan players -> fmap f (io outchan players)

instance Applicative NetworkUno where
  pure x = NetworkUno $ \_ _ -> pure x
  NetworkUno f <*> NetworkUno x = NetworkUno $ \outchan players 
    -> f outchan players <*> x outchan players

instance Monad NetworkUno where
  NetworkUno x >>= f = NetworkUno $ \outchan players -> do
    result <- x outchan players 
    runNetworkUno (f result) outchan players 

instance MonadIO NetworkUno where
    liftIO action = NetworkUno $ \outchan players -> action

instance UnoGame NetworkUno where  
  getPlayerMove pid' board = NetworkUno $ \outchan players -> do
    putStrLn $ "Player's " ++ show pid' ++ " turn!"
    let currentPlayer = getCurrentPlayer board 
    let (Player pid name hand handle chan) = currentPlayer
    broadcastOutGameState (boardToSBoard board) outchan
    _ <- getLine
     
    return []
  
    where
    broadcastOutGameState :: SBoard -> OutChan -> IO ()
    broadcastOutGameState sboard outchan = do
      _broadcast outchan 
        (GameState sboard)  
        All (-1)
    

runNetworkGame :: OutChan -> [(Int, String, Chan Message, Handle)] ->  IO ()
runNetworkGame outchan serverPlayers = do
    putStrLn "starting game!!!"
    let pl = map (\(pid, name, chan, handle) -> createPlayer pid handle chan name) serverPlayers
    let players = Players ([], pl)
    let game' :: NetworkUno (Score, Board)
        game' = game players
    (result, finalBoard) <- runNetworkUno game' outchan players
    -- putStrLn $ "final board: " ++ show finalBoard
    -- putStrLn $ "Score = " ++ show result
    return ()

