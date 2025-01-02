module Main (main) where

import System.Environment

parse :: [String] -> IO ()
parse ["-h"] = putStrLn "Usage: GameServer [-h] [\"server\"|\"client\"]"
parse ["server"] = putStrLn "Starting server..."
parse ["client"] = putStrLn "Starting client..."
parse _ = putStrLn "Not supported."

main :: IO ()
main = getArgs >>= parse
  
   
