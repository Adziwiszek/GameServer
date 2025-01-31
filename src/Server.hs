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
import Uno (runNetworkGame)

type GameStarted = MVar Bool
type ServerPlayers = MVar [(Int, String, Chan Message, Handle)]
type Turn = MVar Int

  
{--
 TODO
 - change it back so every user thread send messages out instead of one sender to all users
 - instead of every player having channel for input have one channel for input
--}

sendOutMsg :: Handle -> Message -> Int -> IO ()
sendOutMsg hdl msg playerId = do
  case messageTarget msg of
    All -> sendMessage hdl msg
    Normal -> unless (playerId == senderID msg) $ sendMessage hdl msg
    ToPlayer targetID -> when (playerId == targetID) $ sendMessage hdl msg
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
runConn (sock, _) chan msgNum gs players _ = do
  let playerId = msgNum
  commLine <- dupChan chan
  inChan <- newChan

  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  sendStr hdl "INIT_ID" playerId

  name <- fmap init (receiveMessage hdl <&> flip unpackStringMessage "default_name")
  putStrLn $ "client name: " ++ name ++ ", client ID = " ++ show playerId  
  

  modifyMVar_ players $ \pl -> return $ (playerId, name, inChan, hdl) : pl

  senderThread <- forkIO $ fix $ \loop -> do
    msg <- readChan commLine
    sendOutMsg hdl msg playerId
    loop

  -- reader thread
  handle (\(SomeException e) -> putStrLn $ "Server error: " ++ show e) $ fix $ \loop -> do
    msg <- receiveMessage hdl
    putStrLn "received message from client"
    case content msg of
      Text _ -> do
        let line = init (unpackStringMessage msg "ERR")
        case line of
          "quit" -> do
            -- sendStr hdl "Bye!" playerID
            writeChan commLine $ Message (ToPlayer playerId) (Text "Bye!") (-1) 
            putStrLn $ "user " ++ name ++ " is quiting.."
          ':' : "start" -> do
            --writeChan commLine $ Message Text All "Starting the game..." (-1)
            modifyMVar_ gs (\_ -> return True)
            _players <- readMVar players 
            _ <- forkIO $ runNetworkGame chan _players 
            loop
          _ -> do
            currentGS <- readMVar gs
            unless currentGS 
                $ writeChan commLine 
                $ Message Normal (Text (name ++ ": " ++ line)) (senderID msg)
            loop
      GameMove m -> do
        currentGS <- readMVar gs
        when currentGS $ do
          -- currentTurn <- readMVar turn
          writeChan inChan (Message Server (GameMove m) playerId)
        loop
      GameState _ -> loop


  killThread senderThread                      
  writeChan chan $ Message Normal (Text ("<-- " ++ name ++ " left.")) playerId
  hClose hdl                             

    
printStrMessage :: Message -> IO ()
printStrMessage msg = case content msg of
  Text s -> putStrLn s
  _      -> return ()

