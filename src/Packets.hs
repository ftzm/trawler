{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Packets (runPcap, Traffic(..)) where

--------------------------------------------------------------------------------
-- Imports

import           Prelude
import           Foreign                        ( Word8
                                                , Ptr
                                                , peekArray
                                                )
import           Network.Pcap                   ( PktHdr
                                                , loop
                                                , setFilter
                                                , setDirection
                                                , openLive
                                                , Direction(..)
                                                , hdrCaptureLength
                                                , Callback
                                                )
import           Data.ByteString.Lazy           ( pack )
import           Data.Binary.Get                ( Get
                                                , getWord8
                                                , getWord16be
                                                , skip
                                                --, runGet
                                                , runGetOrFail
                                                )
import           Control.Concurrent.Chan.Unagi  ( newChan
                                                , InChan
                                                , OutChan
                                                , writeChan
                                                )
import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( void )

--------------------------------------------------------------------------------
-- Types

data LinkHeader = LinkHeader
  deriving (Show)

type IP = (Int, Int, Int, Int)

data IPHeader
  = IPHeader
  { srcIP :: IP
  , destIP :: IP
  , packetSize :: Int
  } deriving (Show)

data TCPHeader
  = TCPHeader
  { srcPort :: Int
  , destPort :: Int
  } deriving (Show)

data PacketDirection = Up | Down deriving (Show)

data Protocol = TCP | UDP deriving (Show)

data Traffic
  = Traffic
  { size :: Int
  , direction :: PacketDirection
  , protocol :: Protocol
  , localPort :: Int
  , remoteIP :: IP
  , remotePort :: Int
  , remoteHostName :: Maybe String
  , processName :: Maybe String
  } deriving (Show)

--------------------------------------------------------------------------------
-- Parse Packet Bytes

getLinkHeader :: Get LinkHeader
getLinkHeader = LinkHeader <$ skip 14

getIP :: Get IP
getIP = (,,,) <$> g <*> g <*> g <*> g
  where g = fromIntegral <$> getWord8

getIPHeader :: Get IPHeader
getIPHeader = do
  skip 2
  size <- fromIntegral <$> getWord16be
  skip 8
  src  <- getIP
  dest <- getIP
  return $ IPHeader src dest size

getTCPHeader :: Get TCPHeader
getTCPHeader = do
  src  <- fromIntegral <$> getWord16be
  dest <- fromIntegral <$> getWord16be
  return $ TCPHeader src dest

parsePacket :: Get Traffic
parsePacket = do
  _              <- getLinkHeader
  IPHeader {..}  <- getIPHeader
  TCPHeader {..} <- getTCPHeader
  return $ Traffic packetSize Down TCP destPort srcIP srcPort Nothing Nothing

--------------------------------------------------------------------------------
-- Pcap

getPacketContent :: PktHdr -> Ptr Word8 -> IO [Word8]
getPacketContent header = peekArray (fromIntegral (hdrCaptureLength header))

asCallback :: ([Word8] -> IO ()) -> Callback
asCallback f h p = getPacketContent h p >>= f

startPcap :: Callback -> IO ()
startPcap f = void $ async $ do
  p <- openLive "wlp61s0" 120 True 500000
  setDirection p In
  setFilter p "tcp" True (fromIntegral @Int 0)
  void $ loop p (-1) f

parseToChan :: InChan Traffic -> Callback
parseToChan chan = asCallback $ \content ->
  case runGetOrFail parsePacket $ pack content of
    Left  _         -> print "failure"
    Right (_, _, t) -> writeChan chan t

--------------------------------------------------------------------------------
-- Interface

runPcap :: IO (OutChan Traffic)
runPcap = do
  (inChan, outChan) <- newChan
  startPcap $ parseToChan inChan
  return outChan
