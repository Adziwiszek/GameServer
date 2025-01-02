module Server (startServer) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import qualified Data.Map as Map

import Message

type Msg = (Int, String)
type RoomName = String
-- zmienna współdzielona przez wątki
type RoomMap = MVar (Map.Map RoomName (Chan Msg)) 

  
{--
 - TODO
 - rozbudować type message o stan gry, można przesyłać albo stan gry albo wiadomość albo ruch (od klienta)
 - dodać limit osób w pokoju, który jest taki jaki jaka jest maks liczba osób do gry
 - przygotować typy dla gry
 - monada TurnBasedGame, GameServer, który będzie jak IOGame, czyli będzie odpowiedzialny
 - za uruchomienie gry na serwerze.
 - --}

startServer :: IO ()
startServer = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  rooms <- newMVar Map.empty 
  putStrLn "Running server on localhost, port = 4242"
  mainLoop sock rooms 0

mainLoop :: Socket -> RoomMap -> Int -> IO ()
mainLoop sock rooms msgNum = do
  putStrLn "waiting for a connection..."
  conn <- accept sock
  putStrLn "connection accepted!"
  _ <- forkIO (runConn conn rooms msgNum)
  mainLoop sock rooms $! msgNum + 1

runConn :: (Socket, SockAddr) -> RoomMap -> Int -> IO ()
runConn (sock, _) rooms msgNum = do
  putStrLn "someone joined!"
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  let welcomeMessage = Message {
    messageType = "msg",
    content = "witaj ufoludzie",
    messageId = 1
  }
  sendMessage hdl welcomeMessage
  hPutStrLn hdl "Welcome to the chat server!"
  hPutStrLn hdl "Enter room name to join or create:"
  roomName <- fmap init (hGetLine hdl)
  putStrLn $ "client wants to go to room: " ++ roomName

  (roomChan, is_admin) <- modifyMVar rooms $ \roomMap -> do
    case Map.lookup roomName roomMap of
      Just chan -> do
        hPutStrLn hdl "Joining room"
        return (roomMap, (chan, False)) -- Pokój istnieje
      Nothing -> do
        hPutStrLn hdl "Creating room"
        chan <- newChan -- Tworzenie nowego pokoju
        return (Map.insert roomName chan roomMap, (chan, True))

  let broadcast msg = writeChan roomChan (msgNum, msg)
  hPutStrLn hdl ("Joined room: " ++ roomName)
  when is_admin (hPutStrLn hdl "you are an admin!")

  hPutStrLn hdl "Hi, what is your name?"
  name <- fmap init (hGetLine hdl)
  putStrLn $ "client name: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.")
  hPutStrLn hdl ("Welcome, " ++ name ++ "!")

  commLine <- dupChan roomChan

  -- fork off a thread for reading from the duplicated channel
  -- ten wątek jest odpowiedzialny za czytanie wiadomości z kanału 
  -- i przesyłanie ich do klienta (jeśli wiadomość nie pochodzi od niego)
  reader <- forkIO $ fix $ \loop -> do
      (nextNum, line) <- readChan commLine
      when (msgNum /= nextNum) $ hPutStrLn hdl line
      loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
      line <- fmap init (hGetLine hdl)
      case line of
        -- If an exception is caught, send a message and break the loop
        "quit" -> do
          hPutStrLn hdl "Bye!"
          putStrLn $ "user " ++ name ++ " is quiting.."
        ':' : rest -> do
          when is_admin (putStrLn $ "user used command " ++ rest)
          hPutStrLn hdl "ads"
          loop
        -- else, continue looping.
        _      -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader                      -- kill after the loop ends
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  hClose hdl                             -- close the handle
