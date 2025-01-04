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
  chan <- newChan
  putStrLn "Running server on localhost, port = 4242"
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop sock chan msgNum = do
  putStrLn "waiting for a connection..."
  conn <- accept sock
  putStrLn "connection accepted!"
  _ <- forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1


runConn :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let playerID = msgNum

  let putInMsg con typ target = Message { messageTarget=target, messageType=typ, content=con, senderID = playerID }
  let broadcast msg typ targ = writeChan chan $ putInMsg msg typ targ
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  sendStr hdl "Hi, what is your name?" playerID
  name <- fmap init (receiveMessage hdl <&> content)
  putStrLn $ "client name: " ++ name
  broadcast ("--> " ++ name ++ " entered chat.") "msg" Normal
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
          sendStr hdl (content newMsg) (senderID newMsg) 
          loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
      line <- fmap init (receiveMessage hdl <&> content)
      case line of
        -- If an exception is caught, send a message and break the loop
        "quit" -> do
          sendStr hdl "Bye!" playerID
          putStrLn $ "user " ++ name ++ " is quiting.."
        ':' : "start" -> do
          broadcast "Starting the game..." "msg" All
          loop
        ':' : rest -> do
          putStrLn $ "command used: " ++ rest
          loop
        -- else, continue looping.
        _      -> broadcast (name ++ ": " ++ line) "msg" Normal >> loop

  killThread reader                      
  broadcast ("<-- " ++ name ++ " left.") "msg" Normal
  hClose hdl                             
