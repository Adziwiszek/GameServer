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
-- zmienna współdzielona przez wątki

  
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
  chan <- newChan
  putStrLn "Running server on localhost, port = 4242"
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  putStrLn "waiting for a connection..."
  conn <- accept sock
  putStrLn "connection accepted!"
  _ <- forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  sendStr hdl "Hi, what is your name?"
  name <- fmap init (receiveMessage hdl <&> content)
  putStrLn $ "client name: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.")
  sendStr hdl ("Welcome, " ++ name ++ "!")

  commLine <- dupChan chan

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
          putStrLn $ "coommand used: " ++ rest
          loop
        -- else, continue looping.
        _      -> broadcast (name ++ ": " ++ line) >> loop

  killThread reader                      -- kill after the loop ends
  broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
  hClose hdl                             -- close the handle
