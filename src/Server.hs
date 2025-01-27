module Server (startServer) where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
-- import Control.Monad (when)
import Control.Monad.State
-- import Control.Monad.Fix (fix)
import Data.Functor
-- import qualified Data.Map as Map

import Message
import Types
import Uno(runGame)

type GameStarted = MVar Bool
type ServerPlayers = MVar [(Int, String, Chan Message, Handle)]
type Turn = MVar Int

  
{--
 TODO
 - change it back so every user thread send messages out instead of one sender to all users
 - instead of every player having channel for input have one channel for input
--}

sendOutMsg :: Handle -> Message -> Int -> IO ()
sendOutMsg hdl msg playerID = do
  case messageTarget msg of
    All -> sendMessage hdl msg
    Normal -> unless (playerID == senderID msg) $ sendMessage hdl msg
    ToPlayer targetID -> when (playerID == targetID) $ sendMessage hdl msg
    Server -> return ()

startServer :: IO ()
startServer = do
  gameStarted <- newMVar False
  players <- newMVar []
  turn <- newMVar (-1)

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  chan <- newChan

  {-_ <- forkIO $ fix $ \loop -> do
    msg <- readChan chan
    _players <- readMVar players
    sendOutAll msg $ map (\(pId, _, hdl) -> (pId, hdl)) _players
    loop-}

  putStrLn "Running server on localhost, port = 4242"
  mainLoop sock chan 0 gameStarted players turn

mainLoop :: Socket -> Chan Message -> Int -> GameStarted -> ServerPlayers -> Turn -> IO ()
mainLoop sock chan msgNum gs players turn = do
  putStrLn "waiting for a connection..."
  conn <- accept sock
  putStrLn "connection accepted!"
  _ <- forkIO (runConn conn chan msgNum gs players turn)
  mainLoop sock chan (msgNum + 1) gs players turn


runConn :: (Socket, SockAddr) -> Chan Message -> Int -> GameStarted -> ServerPlayers -> Turn -> IO ()
runConn (sock, _) chan msgNum gs players turn = do
  let playerID = msgNum
  commLine <- dupChan chan
  inChan <- newChan

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  sendStr hdl "Hi, what is your name?" playerID
  name <- fmap init (receiveMessage hdl <&> flip unpackStringMessage "default_name")
  putStrLn $ "client name: " ++ name ++ ", client ID = " ++ show playerID  
  writeChan chan $ Message Normal (Text ("--> " ++ name ++ " entered chat.")) playerID
  sendStr hdl ("Welcome, " ++ name ++ "!") playerID


  -- adding this players handle to the players list
  modifyMVar_ players $ \pl -> return $ (playerID, name, inChan, hdl) : pl
  sendStr hdl "INIT_ID" playerID

  -- fork off a thread for reading from the duplicated channel
  -- ten wątek jest odpowiedzialny za czytanie wiadomości z kanału 
  -- i przesyłanie ich do klienta (jeśli wiadomość nie pochodzi od niego)
  reader <- forkIO $ fix $ \loop -> do
    msg <- readChan commLine
    sendOutMsg hdl msg playerID
    loop

  -- handle odpowiada za odczytywanie wiadomości od użytkownika i broadcastowanie
  -- ich do reszty użytkowników
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
    msg <- receiveMessage hdl
    let line = init (unpackStringMessage msg "ERR")
    case line of
      "quit" -> do
        sendStr hdl "Bye!" playerID
        putStrLn $ "user " ++ name ++ " is quiting.."
      ':' : "start" -> do
        --writeChan commLine $ Message Text All "Starting the game..." (-1)
        modifyMVar_ gs (\_ -> return True)
        _players <- readMVar players 
        _ <- forkIO $ runGame chan _players 
        loop
      ':' : rest -> do
        putStrLn $ "command used: " ++ rest
        loop
      _ -> do
        currentGS <- readMVar gs
        if currentGS then do
          currentTurn <- readMVar turn
          when (currentTurn == playerID) $ writeChan inChan $ Message Server (Text line) playerID
        else
          writeChan commLine $ Message Normal (Text (name ++ ": " ++ line)) (senderID msg)
        loop

  killThread reader                      
  writeChan chan $ Message Normal (Text ("<-- " ++ name ++ " left.")) playerID
  hClose hdl                             
