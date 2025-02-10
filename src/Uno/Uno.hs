module Uno.Uno (game, runNetworkGame, parseCards, defaultCard) where 

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Control.Monad.Cont
import Control.Concurrent.STM 
import Control.Monad.Random
import System.IO
-- import Control.Monad.Fix (fix)
import Control.Concurrent
-- import Control.Monad (when)

import Types
import Utils
import Uno.Defaults
import Uno.Types
import Uno.Common.Types
import Uno.Utils
import Message


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
    let (left, p, right) = unpackPlayers $ boardPlayers board 
    in let cards = playerHand p
    in let newHand = foldr 
              (\c acc -> if c `cardMember` move then acc else c : acc) 
              [] 
              cards
    in board {boardPlayers=Players (left, p {playerHand=newHand}:right)}

  where 
    unpackPlayers (Players (left, p:right)) = (left, p, right)
    unpackPlayers _ = undefined


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
      let toDraw = max 1 (addToPlayer board')
      drawnBoard <- addToCurrentPlayer board' toDraw
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
  sendStartingInfo startingBoard
  play startingBoard
  where         
    play :: (MonadIO m, UnoGame m) => Board -> m (Score, Board)
    play board = do
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


boardToSBoard :: Board -> Int -> SBoard
boardToSBoard b pid = 
  let thisPlayer = getPlayerWithID b pid in
  let otherPlayers_ =  SPlayers $ map toSPlayer $ getAllPlayersList b in
  let discardedCard_ = head $ discardPile b in 
  let myHand_ = playerHand thisPlayer in
  let sdirection_ = direction b in
  let name = playerName thisPlayer in
  let toDraw = addToPlayer b in
  let currentPlayer = getCurrentPlayer b in
  SBoard pid name otherPlayers_ discardedCard_ sdirection_ myHand_ (playerID currentPlayer, playerName currentPlayer) toDraw
       
  where 
    toSPlayer (Player pid' name hand _ _) = SPlayer 
      { splayerName = name
      , sid = pid'
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


createPlayer :: Int -> Handle -> TChan Message -> String -> Player
createPlayer pid han chan name = Player pid name [] han chan 

-- type InChan = Chan Message
type OutChan = TChan Message
-- type Players = [(Int, Chan Message, Handle)]
-- _type Turn = MVar Int
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
    liftIO action = NetworkUno $ \_ _ -> action


instance UnoGame NetworkUno where  
  sendStartingInfo board = NetworkUno $ \outchan _ -> do
      let sboards = map (boardToSBoard board . playerID) $ getAllPlayersList board
      mapM_ (`broadcastStartingInfo` outchan) sboards

    where
      broadcastStartingInfo :: SBoard -> TChan Message -> IO ()
      broadcastStartingInfo sboard outchan = do
        let (SPlayers otherplayers) = otherPlayers sboard
        _broadcast outchan (StartingGameInfo otherplayers) (ToPlayer $ myID sboard) (-1)
    
  getPlayerMove _ board = NetworkUno $ \outchan _ -> do
    let (Player pid _ _ _ inchan) = getCurrentPlayer board 


    let sboards = map (boardToSBoard board . playerID) $ getAllPlayersList board
    mapM_ (`broadcastOutGameState` outchan) sboards
     
    move <- fix $ \loop -> do
      testMsg <- atomically $ readTChan inchan 
      if senderID testMsg == pid 
      then maybe loop return (processUserMsg $ content testMsg)
      else do
        _broadcast outchan (Text "Move from wrong player!") All 0
        loop
    putStrLn $ "move  = " ++ show move
    return move
  
    where
    broadcastOutGameState :: SBoard -> OutChan -> IO ()
    broadcastOutGameState sboard outchan = do
      _broadcast outchan (GameState sboard) (ToPlayer $ myID sboard) (-1)

    processUserMsg :: MessageContent -> Maybe [Card]
    processUserMsg (GameMove []) = Nothing
    processUserMsg (GameMove move) = Just move
    processUserMsg _ = Nothing

    

runNetworkGame :: OutChan -> [(Int, String, TChan Message, Handle)] ->  IO ()
runNetworkGame outchan serverPlayers = do
    putStrLn "starting game!!!"
    let pl = map (\(pid, name, chan, handle) -> createPlayer pid handle chan name) serverPlayers
    let players = Players ([], pl)
    let game' :: NetworkUno (Score, Board)
        game' = game players
    (_, finalBoard) <- runNetworkUno game' outchan players
    putStrLn $ "final board: " ++ show finalBoard
    --putStrLn $ "Score = " ++ show result
    return ()


