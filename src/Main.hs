module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)

import Client
import Server
import Ui.Client as UIClient

data AppType 
  = Server
  | Client
  | Tooltip
  | GraphicsClient String

main :: IO ()
main = do
  appType <- getArgs >>= parse
  case appType of 
    Server -> startServer
    Client -> startClient
    Tooltip -> return ()
    GraphicsClient username -> UIClient.runGraphicsClient username

parse :: [String] -> IO AppType
parse ["-h"] = do
  putStrLn "Usage: GameServer [-h] [\"server\"|\"client\"]"
  return Tooltip
parse ["server"] = do
  putStrLn "Starting server...!"
  return Server
parse ["client"] = do 
  putStrLn "Starting client..."
  return Client
parse ["g"] = do
  putStr "Enter your name: "
  hFlush stdout
  username <- getLine
  putStrLn "Launching game...\nPress 'q' to quit"
  return $ GraphicsClient username
parse _ = do
  putStrLn "Not supported. Try 'stack run client' or 'stack run server'."
  return Tooltip


