module Client (startClient, startUiClient) where

import Network.Socket
import System.IO
import Control.Exception (bracket, handle, SomeException(..), displayException)
import Control.Monad.Fix (fix)
import Control.Concurrent
import System.Timeout
import Control.Monad (when)
-- import System.Console.ANSI (clearScreen)
import System.Process (callCommand)

import Message
import Types
import Uno

type PlayerID = MVar Int

type MessageChan = Chan Message

startClient :: IO ()
startClient = do
    let host = "127.0.0.1"
    let port = "4242"
    putStrLn "Attempting to connect..."
    playerId <- newEmptyMVar
    -- Use bracket to ensure proper cleanup
    bracket (connect' host port) cleanup (`handleConnection` playerId)
    where
        connect' host port = do
            addr <- resolve host port
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock (addrAddress addr)
            hdl <- socketToHandle sock ReadWriteMode
            hSetBuffering hdl NoBuffering
            -- testing message 
            {-welcomeMessage <- receiveMessage hdl
            putStrLn $ content welcomeMessage-}
            return (sock, hdl)
            
        cleanup (sock, hdl) = do
            hClose hdl
            close sock

handleConnection :: (Socket, Handle) -> PlayerID -> IO ()
handleConnection (_, hdl) playerId = do
  -- channel for passing messages between threads
  messageChan <- newChan

  readerThread <- forkIO $ fix $ \loop -> do
    msg <- receiveMessage hdl
    case content msg of
      Text "INIT_ID" -> do
        wasINIT <- isEmptyMVar playerId
        if wasINIT then do
          putMVar playerId $ senderID msg
          putStrLn $ "your id = " ++ show (senderID msg)
          loop
        else loop
      -- when we get Bye message we signal main thread to finish
      Text "Bye!" -> do
        putStrLn "Bye!" 
        writeChan messageChan "DISCONNECT"
      GameState gs -> do
        --clearScreen
        callCommand "clear"
        -- putStrLn $ show gs
        print gs
        loop
      Text s -> do 
        putStrLn s
        loop
      GameMove _ -> do
        putStrLn "dupa kozy"
        loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    msg <- getLine
    myId <- readMVar playerId
    parsedMsg <- parseUserMessage msg
    case parsedMsg of 
      Nothing -> loop 
      Just m -> do
        sendToServer hdl m myId
        -- sendStr hdl (msg ++ " ") myID 
        case msg of
          "quit" -> do
            result <- timeout 500000 $ readChan messageChan
            case result of
              Nothing -> putStrLn "Server not responding. Forcing disconnect..."
              Just _ -> return ()
          _ -> loop

  killThread readerThread
  hClose hdl

resolve :: String -> String -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

-- utils

parseUserMessage :: String -> IO (Maybe MessageContent)
parseUserMessage "" = return Nothing
parseUserMessage msg = do
  let msgWords = words msg
  case msgWords of
    [] -> return $ Just $ Text ""
    "|" : rest -> do
      let cards = parseCards $ joinStr rest " "
      return $ Just $ GameMove cards
    _ -> do
      return $ Just $ Text $ msg ++ " "

-- inchan -> input channel, messages that come from server
-- outchan -> output channel, messages that we want to send to the server

startUiClient :: MessageChan -> MessageChan -> PlayerID -> String -> IO ()
startUiClient inchan outchan playerId pName = do
  let host = "127.0.0.1"
  let port = "4242"
  putStrLn "Attempting to connect..."

  writeChan outchan $ Message Server (Text (pName ++ "_"))  (-1)

  -- Use bracket to ensure proper cleanup
  bracket (connect' host port) cleanup (\x -> handleConnection2 x inchan outchan playerId)
  where
    connect' host port = do
        addr <- resolve host port
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        hdl <- socketToHandle sock ReadWriteMode
        hSetBuffering hdl NoBuffering
        return (sock, hdl)
        
    cleanup (sock, hdl) = do
        hClose hdl
        close sock


handleConnection2 :: (Socket, Handle) -> MessageChan -> MessageChan -> PlayerID -> IO ()
handleConnection2 (_, hdl) inchan outchan playerId = do
  -- channel for passing messages between threads
  messageChan <- newChan

  readerThread <- forkIO $ fix $ \loop -> do
    msg <- receiveMessage hdl
    if connectionEnded msg
    then return ()
    else do
      case content msg of 
        Text "INIT_ID" -> initPlayerId msg
        _ -> return ()
      -- we write to inchan and let main event loop deal with this message
      writeChan inchan msg
      loop

  -- sender thread
  handle handleSenderThreadException $ fix $ \loop -> do
    msg <- readChan outchan
    myId <- readMVar playerId
    sendToServer hdl msg myId
    -- check if we want to disconnect from the server (right now clunky)
    case content msg of
      Text "quit" -> do
        result <- timeout 500000 $ readChan messageChan
        case result of
          Nothing -> putStrLn "Server not responding. Forcing disconnect..."
          Just _ -> return ()
      _ -> loop

  killThread readerThread
  hClose hdl

  where 
    connectionEnded msg = case content msg of
      Text "Bye!" -> True
      _           -> False

    handleSenderThreadException (SomeException e) = do
      print e

    initPlayerId msg = case content msg of
      Text "INIT_ID" -> do
        idEmpty <- isEmptyMVar playerId
        when idEmpty $ do
          putMVar playerId $ senderID msg
      _ -> return ()

printStrMessage :: Message -> IO ()
printStrMessage msg = case content msg of
  Text s -> putStrLn s
  _      -> return ()
