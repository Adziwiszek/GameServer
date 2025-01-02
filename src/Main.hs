module Main (main) where

import System.Environment

import Server

data AppType 
  = Server
  | Client
  | Tooltip

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
parse _ = do
  putStrLn "Not supported."
  return Tooltip

main :: IO ()
main = do
  appType <- getArgs >>= parse
  case appType of 
    Server -> startServer
    _ -> putStrLn "not implemeted"
  
   
