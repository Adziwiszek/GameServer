{-# LANGUAGE DeriveGeneric #-}

module Message (module Message) where 

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Int
import System.IO

data Message = Message {
  messageType :: String,
  content :: String,
  messageId :: Int
} deriving (Generic, Show)

instance Binary Message 

sendMessage :: Handle -> Message -> IO ()
sendMessage hdl msg = do
  let encoded = encode msg
  BSL.hPut hdl (encode (BSL.length encoded))
  BSL.hPut hdl encoded
  hFlush hdl

receiveMessage :: Handle -> IO Message 
receiveMessage hdl = do
  sizeBS <- BSL.hGet hdl 8
  let size = decode  sizeBS :: Int64
  msgBS <- BSL.hGet hdl (fromIntegral size)
  return $ decode msgBS 
