module Main (main) where

import System.Environment
import Control.Monad(unless)
import Data.Text (Text)
import qualified Data.Text as T
import SDL


import Client
import Server
import Uno

data AppType 
  = Server
  | Client
  | Tooltip
  | Uno
  | Graphics

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
  return Graphics
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
    Graphics -> do
      window <- createWindow (T.pack "My SDL Application") defaultWindow
      renderer <- createRenderer window (-1) defaultRenderer
      appLoop renderer
      destroyWindow window
    -- _ -> putStrLn "not implemeted"
  
appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer) 
