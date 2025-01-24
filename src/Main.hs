module Main (main) where

import System.Environment

import Client
import Server
import Uno
import Gui

data AppType 
  = Server
  | Client
  | Tooltip
  | Uno
  | GraphicsClient

parse :: [String] -> IO AppType
parse ["-h"] = do
  putStrLn "Usage: GameServer [-h] [\"server\"|\"client\"]"
  return Tooltip
parse ["server"] = do
  putStrLn "Starting server..."
  return Server
parse ["client"] = do 
  putStrLn "Starting client..."
  return Client
parse ["testuno"] = do 
  putStrLn "testing uno..."
  return Uno
parse ["g"] = do
  putStrLn "Launching game...\nPress 'q' to quit"
  return GraphicsClient
parse _ = do
  putStrLn "Not supported."
  return Tooltip

main :: IO ()
main = do
  appType <- getArgs >>= parse
  case appType of 
    Server -> startServer
    Client -> startClient
    Uno     -> runGame
    Tooltip -> return ()
    GraphicsClient -> runSlots
