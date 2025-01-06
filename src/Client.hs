module Client (startClient) where

import Network.Socket
import System.IO
import Control.Exception (bracket, try, handle, SomeException(..))
import Control.Monad.Fix (fix)
import Control.Concurrent
import System.Timeout
import System.Console.ANSI (clearScreen)
import System.Process (callCommand)

import Message

type PlayerID = MVar Int

startClient :: IO ()
startClient = do
    let host = "127.0.0.1"
    let port = "4242"
    putStrLn "Attempting to connect..."
    playerID <- newEmptyMVar
    -- Use bracket to ensure proper cleanup
    bracket (connect' host port) cleanup (flip handleConnection playerID)
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
handleConnection (_, hdl) playerID = do
  -- channel for passing messages between threads
  messageChan <- newChan

  readerThread <- forkIO $ fix $ \loop -> do
    msg <- receiveMessage hdl
    case content msg of
      "INIT_ID" -> do
        wasINIT <- isEmptyMVar playerID
        if wasINIT then do
          putMVar playerID $ senderID msg
          putStrLn $ "your id = " ++ show (senderID msg)
          loop
        else loop
      -- when we get Bye message we signal main thread to finish
      "Bye!" -> do
        putStrLn $ content msg
        writeChan messageChan "DISCONNECT"
      _ -> do 
        case messageType msg of 
          GameState -> do
            --clearScreen
            callCommand "clear"
            putStrLn $ content msg
            loop
          _ -> do
            putStrLn $ content msg
            loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    msg <- getLine
    myID <- readMVar playerID
    sendStr hdl (msg ++ " ") myID 
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
