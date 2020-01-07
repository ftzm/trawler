{-# LANGUAGE TypeApplications #-}

module Packets where

import Prelude
import Foreign (Word8, Ptr, peekArray)
import Network.Pcap (PktHdr, loop, setFilter, setDirection, openLive, datalink, Direction(..), hdrCaptureLength)
import Data.ByteString.Lazy (pack)
import Data.Binary.Get (Get, getWord8, getWord16be, skip, runGet)

getPacketContent :: PktHdr -> Ptr Word8 -> IO [Word8]
getPacketContent header pointer =
  peekArray (fromIntegral (hdrCaptureLength header)) pointer

pcapGuy :: IO ()
pcapGuy = do
  p <- openLive "wlp61s0" 65535 True 0

  print =<< datalink p

  --setFilter p "ip or udp or tcp" True (fromIntegral 0)
  setDirection p In
  setFilter p "tcp" True (fromIntegral @Int 0)
  _ <- loop p (-1) printIt
  return ()
  --count <- dispatch p 1 pf
  --putStrLn $ "packets read " ++ (show count)
  --return ()

printIt :: PktHdr -> Ptr Word8 -> IO ()
printIt header pointer = do
    content <- getPacketContent header pointer
    print $ runGet parsePacket $ pack $ content

data LinkHeader = LinkHeader
  deriving (Show)

type IP = (Int, Int, Int, Int)

data IPHeader
  = IPHeader
  { srcIP :: IP
  , destIP :: IP
  } deriving (Show)

data TCPHeader
  = TCPHeader
  { srcPort :: Int
  , destPort :: Int
  } deriving (Show)

getLinkHeader :: Get LinkHeader
getLinkHeader = const LinkHeader <$> skip 14

getIP :: Get IP
getIP = do
  one <- fromIntegral <$> getWord8
  two <- fromIntegral <$> getWord8
  three <- fromIntegral <$> getWord8
  four <- fromIntegral <$> getWord8
  return $ (one, two, three, four)

getIPHeader :: Get IPHeader
getIPHeader = do
  skip 12
  src <- getIP
  dest <- getIP
  return $ IPHeader src dest

getTCPHeader :: Get TCPHeader
getTCPHeader = do
  src <- fromIntegral <$> getWord16be
  dest <- fromIntegral <$> getWord16be
  return $ TCPHeader src dest

parsePacket :: Get (LinkHeader, IPHeader, TCPHeader)
parsePacket = (,,) <$> getLinkHeader <*> getIPHeader <*> getTCPHeader

main :: IO ()
main = do
  pcapGuy
  -- B.putStrLn dnsSample
  -- print $ runGet getHeader $ BL.fromStrict dnsSample

--   parseOnly parser dnsSample
