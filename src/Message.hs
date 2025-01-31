module Message (module Message) where 

import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Data.Binary
import Data.Int
import System.IO

import Types

class MessageSender a where
  sendToServer :: Handle -> a -> Int -> IO ()

{-data MessageType
  = Text
  | GameState
  | PlayerMove
  deriving (Generic, Show) -}


_broadcast :: Chan Message -> MessageContent -> MessageTarget -> Int -> IO ()
_broadcast chan msg targ sID = writeChan chan $ Message {
    messageTarget=targ,
    content=msg,
    senderID=sID
  }

sendToPlayer :: Chan Message -> Int -> MessageContent -> IO ()
sendToPlayer chan playerId msgContent = do
  let msg = Message {messageTarget=ToPlayer playerId, content=msgContent, senderID= -1}
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
  -- putStrLn "trying to receive message..."
  sizeBS <- BSL.hGet hdl 8
  let size = decode  sizeBS :: Int64
  -- putStrLn $ "received message size = " ++ show size
  msgBS <- hGetExact hdl size
  -- putStrLn "received message content"
  return $ decode msgBS 

smsg :: String -> Int -> Message
smsg str mId = Message {
  content = Text str,
  senderID = mId,
  messageTarget = All
}

sendStr :: Handle -> String -> Int -> IO ()
sendStr hdl str mId = sendMessage hdl $ smsg str mId

instance MessageSender MessageContent where
  sendToServer hdl cont pId = do
    sendMessage hdl $ Message Server cont pId

instance MessageSender Message where
  sendToServer hdl msg _ = do
    sendMessage hdl msg
    
unpackStringMessage :: Message -> String -> String 
unpackStringMessage msg def = process $ content msg where
  process (GameState _) = def
  process (Text s)      = s
  process (GameMove _)  = def

