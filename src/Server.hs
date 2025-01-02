module Server (startServer) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Functor
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
  sendStr hdl "witaj ufoludzie"
  sendStr hdl "Welcome to the chat server!\nEnter room name to join or create:"
  -- receiveMessage hdl >>= return . content 
  roomName <- fmap init (receiveMessage hdl <&> content) 
  putStrLn $ "client wants to go to room: " ++ roomName

  (roomChan, is_admin) <- modifyMVar rooms $ \roomMap -> do
    case Map.lookup roomName roomMap of
      Just chan -> do
        sendStr hdl "Joining room"
        return (roomMap, (chan, False)) -- Pokój istnieje
      Nothing -> do
        sendStr hdl "Creating room"
        chan <- newChan -- Tworzenie nowego pokoju
        return (Map.insert roomName chan roomMap, (chan, True))

  let broadcast msg = writeChan roomChan (msgNum, msg)
  sendStr hdl ("Joined room: " ++ roomName)
  when is_admin (sendStr hdl "you are an admin!")

  sendStr hdl "Hi, what is your name?"
  name <- fmap init (receiveMessage hdl <&> content)
  putStrLn $ "client name: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.")
  sendStr hdl ("Welcome, " ++ name ++ "!")

  commLine <- dupChan roomChan

  -- fork off a thread for reading from the duplicated channel
  -- ten wątek jest odpowiedzialny za czytanie wiadomości z kanału 
  -- i przesyłanie ich do klienta (jeśli wiadomość nie pochodzi od niego)
  reader <- forkIO $ fix $ \loop -> do
      (nextNum, line) <- readChan commLine
      when (msgNum /= nextNum) $ sendStr hdl line
      loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
      line <- fmap init (receiveMessage hdl <&> content)
      case line of
        -- If an exception is caught, send a message and break the loop
        "quit" -> do
          sendStr hdl "Bye!"
          putStrLn $ "user " ++ name ++ " is quiting.."
        ':' : rest -> do
          when is_admin (putStrLn $ "user used command " ++ rest)
          sendStr hdl "ads"
          loop
        -- else, continue looping.
        _      -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader                      -- kill after the loop ends
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  hClose hdl                             -- close the handle
