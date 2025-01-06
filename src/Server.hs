module Server (startServer) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Fix (fix)
import Data.Functor
import qualified Data.Map as Map

import Message
import Game(runGame)

type Msg = (Int, String)
type RoomName = String
-- zmienna współdzielona przez wątki
type RoomMap = MVar (Map.Map RoomName (Chan Msg)) 

type GameStarted = MVar Bool
type Players = MVar [(Int, Handle)]

  
{--
 TODO
 - refactoring
 - wysyłanie wyniku do graczy i powrót do lobby
 - wysyłanie z serwera stanu gry i rysowanie po stronie klienta
 - używanie messageType do obsługi typu wiadomości, dla ruch gracza, polecenia dla/od serwera itp
 - osobny kanał na wiadomości i na ruchy graczy
 - wysyłanie po wejściu do gracza jego ID (poprzez polecenie od serwera)
 - walidacja czy ruch gracza jest tym, którego oczekujemy (żeby ten sam graczn nie robił ciągle ruchów)
--}

sendOut :: Message -> [(Int, Handle)] -> IO ()
sendOut msg players = do
  case messageTarget msg of
    All -> mapM_ ((`sendMessage` msg) . snd) players
    Normal -> mapM_ ((`sendMessage` msg) . snd) $ filter (\(hId, _) -> hId /= senderID msg) players
    ToPlayer targetID -> 
      case lookup targetID players of
        Nothing -> putStrLn $ "Can't find player with id = " ++ show targetID
        Just hdl -> sendMessage hdl msg
    Server -> return ()

startServer :: IO ()
startServer = do
  gameStarted <- newMVar False
  players <- newMVar []

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  chan <- newChan

  _ <- forkIO $ fix $ \loop -> do
    msg <- readChan chan
    _players <- readMVar players
    sendOut msg _players
    loop

  putStrLn "Running server on localhost, port = 4242"
  mainLoop sock chan 0 gameStarted players

mainLoop :: Socket -> Chan Message -> Int -> GameStarted -> Players -> IO ()
mainLoop sock chan msgNum gs players = do
  putStrLn "waiting for a connection..."
  conn <- accept sock
  putStrLn "connection accepted!"
  _ <- forkIO (runConn conn chan msgNum gs players)
  mainLoop sock chan (msgNum + 1) gs players

runConn :: (Socket, SockAddr) -> Chan Message -> Int -> GameStarted -> Players -> IO ()
runConn (sock, _) chan msgNum gs players = do
  let playerID = msgNum

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  -- adding this players handle to the players list
  modifyMVar_ players $ \pl -> return $ (playerID, hdl) : pl
  sendStr hdl "INIT_ID" playerID

  sendStr hdl "Hi, what is your name?" playerID
  name <- fmap init (receiveMessage hdl <&> content)
  putStrLn $ "client name: " ++ name ++ ", client ID = " ++ show playerID  
  writeChan chan $ Message Text Normal ("--> " ++ name ++ " entered chat.") playerID
  sendStr hdl ("Welcome, " ++ name ++ "!") playerID

  commLine <- dupChan chan
  inChan <- newChan

  -- fork off a thread for reading from the duplicated channel
  -- ten wątek jest odpowiedzialny za czytanie wiadomości z kanału 
  -- i przesyłanie ich do klienta (jeśli wiadomość nie pochodzi od niego)
  reader <- forkIO $ fix $ \loop -> do
    {-msg <- readChan commLine
    _players <- readMVar players
    sendOut msg _players-}
    loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
    msg <- receiveMessage hdl
    let line = init (content msg)
    case line of
      "quit" -> do
        sendStr hdl "Bye!" playerID
        putStrLn $ "user " ++ name ++ " is quiting.."
      ':' : "start" -> do
        --writeChan commLine $ Message Text All "Starting the game..." (-1)
        modifyMVar_ gs (\_ -> return True)
        _players <- readMVar players 
        _ <- forkIO $ runGame inChan chan _players
        loop
      ':' : rest -> do
        putStrLn $ "command used: " ++ rest
        loop
      _ -> do
        currentGS <- readMVar gs
        if currentGS then do
          putStrLn $ "Player with Id = " ++ show playerID ++ " made a move = " ++ line
          writeChan inChan $ Message Text Server line (senderID msg)
        else
          writeChan commLine $ Message Text Normal (name ++ ": " ++ line) (senderID msg)
        loop

  killThread reader                      
  writeChan chan $ Message Text Normal ("<-- " ++ name ++ " left.") playerID
  hClose hdl                             
