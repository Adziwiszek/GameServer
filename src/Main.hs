module Main (main) where

import System.Environment

import Client
import Server
import Uno

data AppType 
  = Server
  | Client
  | Tooltip
  | Uno

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
    -- _ -> putStrLn "not implemeted"
  
   
