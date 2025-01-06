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
type Players = MVar [Handle]

  
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

startServer :: IO ()
startServer = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    _ <- readChan chan
    loop
  putStrLn "Running server on localhost, port = 4242"
  gameStarted <- newMVar False
  players <- newMVar []
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

  let putInMsg con typ target = Message { messageTarget=target, messageType=typ, content=con, senderID = playerID }
  let broadcast msg typ targ = writeChan chan $ putInMsg msg typ targ
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  -- adding this players handle to the players list
  modifyMVar_ players $ \pl -> return $ hdl : pl
  sendStr hdl "INIT_ID" playerID

  sendStr hdl "Hi, what is your name?" playerID
  name <- fmap init (receiveMessage hdl <&> content)
  putStrLn $ "client name: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.") Text Normal
  sendStr hdl ("Welcome, " ++ name ++ "!") playerID

  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  -- ten wątek jest odpowiedzialny za czytanie wiadomości z kanału 
  -- i przesyłanie ich do klienta (jeśli wiadomość nie pochodzi od niego)
  reader <- forkIO $ fix $ \loop -> do
    newMsg <- readChan commLine
    case messageTarget newMsg of
      Normal -> do
        when (msgNum /= senderID newMsg) $ sendStr hdl (content newMsg) (senderID newMsg)
        loop
      All -> do
        sendMessage hdl $ putInMsg (content newMsg) (messageType newMsg) All
        --sendStr hdl (content newMsg) (senderID newMsg) 
        loop
      Server -> loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
    msg <- receiveMessage hdl
    putStrLn $ "Message from player with ID = " ++ show (senderID msg)
    let line = init (content msg)
    case line of
      "quit" -> do
        sendStr hdl "Bye!" playerID
        putStrLn $ "user " ++ name ++ " is quiting.."
      ':' : "start" -> do
        broadcast "Starting the game..." Text All
        modifyMVar_ gs (\_ -> return True)
        _players <- readMVar players 
        _ <- forkIO $ runGame chan _players
        loop
      ':' : rest -> do
        putStrLn $ "command used: " ++ rest
        loop
      _ -> do
        currentGS <- readMVar gs
        if currentGS then
          broadcast line Text Server 
        else
          broadcast (name ++ ": " ++ line) Text Normal
        loop

  killThread reader                      
  broadcast ("<-- " ++ name ++ " left.") Text Normal
  hClose hdl                             
