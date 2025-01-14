{-# LANGUAGE DeriveGeneric #-}

module Message (module Message) where 

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Data.Binary
import Data.Int
import System.IO

data Player = PlayerA | PlayerB deriving (Eq, Generic)
instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"
instance Binary Player

newtype Board = Board [Maybe Player] deriving Generic
instance Show Board where
  show (Board board) = 
    foldl (\acc (i, x) -> acc ++ " " ++
      maybe " " show x ++
      (if i `mod` 3 == 0 || i `mod` 3 == 1 
        then " |" 
        else 
        if i < 8 then " \n-----------\n"
        else "\n")) -- end of line 
    "\n\n" $ zip [0..] board
instance Binary Board

{-data MessageType
  = Text
  | GameState
  | PlayerMove
  deriving (Generic, Show) -}

data MessageContent
  = Text String
  | GameState Board
  deriving (Generic, Show)

data MessageTarget 
  = All
  | Normal -- don't send ones message to themselves
  | Server -- don't send to players 
  | ToPlayer Int -- send to a specific player
  deriving (Generic, Show) 

data Message = Message {
  messageTarget :: MessageTarget,
  content :: MessageContent,
  senderID :: Int
} deriving (Generic, Show)

instance Binary MessageTarget
instance Binary MessageContent
instance Binary Message 

_broadcast :: Chan Message -> MessageContent -> MessageTarget -> Int -> IO ()
_broadcast chan msg targ sID = writeChan chan $ Message {
    messageTarget=targ,
    content=msg,
    senderID=sID
  }

sendToPlayer :: Chan Message -> Int -> MessageContent -> IO ()
sendToPlayer chan playerID msgContent = do
  let msg = Message {messageTarget=ToPlayer playerID, content=msgContent, senderID= -1}
  writeChan chan msg

sendMessage :: Handle -> Message -> IO ()
sendMessage hdl msg = do
  let encoded = encode msg
  BSL.hPut hdl (encode (BSL.length encoded))
  hFlush hdl
  --putStrLn $ "sent message size" ++ show (BSL.length encoded)
  --putStrLn $ "sent message content = " ++ content msg
  BSL.hPut hdl encoded
  hFlush hdl

hGetExact :: Handle -> Int64 -> IO BSL.ByteString
hGetExact hdl = go BSL.empty 
  where 
    go acc 0 = return acc
    go acc n = do
      chunk <- BSL.hGet hdl (fromIntegral n)
      go (acc <> chunk) (n - fromIntegral (BSL.length chunk))

receiveMessage :: Handle -> IO Message 
receiveMessage hdl = do
  sizeBS <- BSL.hGet hdl 8
  let size = decode  sizeBS :: Int64
  --putStrLn $ "received message size = " ++ show size
  msgBS <- hGetExact hdl size
  --putStrLn "received message content"
  return $ decode msgBS 

smsg :: String -> Int -> Message
smsg str mId = Message {
  content = Text str,
  senderID = mId,
  messageTarget = All
}

sendStr :: Handle -> String -> Int -> IO ()
sendStr hdl str mId = sendMessage hdl $ smsg str mId

unpackStringMessage :: Message -> String -> String 
unpackStringMessage msg def = process $ content msg where
  process (GameState _) = def
  process (Text s)      = s
