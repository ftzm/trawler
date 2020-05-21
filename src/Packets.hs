{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Packets
  ( runPcap
  , Traffic(..)
  , PacketDirection(..)
  , IP
  )
where

--------------------------------------------------------------------------------
-- Imports

import           Prelude hiding (lookup)
import           Foreign                        ( Word8
                                                , Ptr
                                                , peekArray
                                                )
import           Network.Pcap                   ( PktHdr
                                                , loop
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
                                                , runGetOrFail
                                                )
import           Control.Concurrent.Chan.Unagi  ( newChan
                                                , InChan
                                                , OutChan
                                                , writeChan
                                                )
import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( void )
import           Data.Map                       ( empty
                                                , Map
                                                , insert
                                                , lookup
                                                )
import           Data.Word                      ( Word32 )
import           Network.Info                   ( getNetworkInterfaces
                                                , NetworkInterface(..)
                                                , IPv4(..)
                                                )
import           Data.List                      ( foldl' )
import           Data.Bits                      ( shiftR )

--------------------------------------------------------------------------------
-- Types

data LinkHeader = LinkHeader
  deriving (Show)

type IP = (Word8, Word8, Word8, Word8)

data IPHeader
  = IPHeader
  { srcIP :: IP
  , destIP :: IP
  , protocol :: Protocol
  , size :: Int
  } deriving (Show)

data TCPHeader
  = TCPHeader
  { srcPort :: Int
  , destPort :: Int
  } deriving (Show)

data UDPHeader
  = UDPHeader
  { srcPort :: Int
  , destPort :: Int
  } deriving (Show)

data PacketDirection = PacketUp | PacketDown deriving (Show, Eq)

data Protocol = TCP | UDP | UnknownProtocol deriving (Show, Eq)

data Traffic
  = Traffic
  { size :: Int
  , direction :: PacketDirection
  , protocol :: Protocol
  , localIP :: IP
  , localPort :: Int
  , remoteIP :: IP
  , remotePort :: Int
  , remoteHostname :: Maybe String
  , processName :: String
  } deriving (Show)

--------------------------------------------------------------------------------
-- Parse Packet Bytes

byteToProtocol :: Int -> Protocol
byteToProtocol 6  = TCP
byteToProtocol 17 = UDP
byteToProtocol _  = UnknownProtocol

getLinkHeader :: Get LinkHeader
getLinkHeader = LinkHeader <$ skip 14

getIP :: Get IP
getIP = (,,,) <$> g <*> g <*> g <*> g where g = fromIntegral <$> getWord8

getIPHeader :: Get IPHeader
getIPHeader = do
  skip 2
  size <- fromIntegral <$> getWord16be
  skip 5
  protocol <- byteToProtocol . fromIntegral <$> getWord8
  skip 2
  src  <- getIP
  dest <- getIP
  return $ IPHeader src dest protocol size

getTCPHeader :: Get TCPHeader
getTCPHeader = do
  src  <- fromIntegral <$> getWord16be
  dest <- fromIntegral <$> getWord16be
  return $ TCPHeader src dest

getUDPHeader :: Get UDPHeader
getUDPHeader = do
  src  <- fromIntegral <$> getWord16be
  dest <- fromIntegral <$> getWord16be
  return $ UDPHeader src dest

parsePacket :: IP -> Get Traffic
parsePacket localIP = do
  _                   <- getLinkHeader
  IPHeader {..}       <- getIPHeader
  (srcPort, destPort) <- case protocol of
    TCP -> do
      TCPHeader {..} <- getTCPHeader
      return (srcPort, destPort)
    UDP -> do
      UDPHeader {..} <- getUDPHeader
      return (srcPort, destPort)
    UnknownProtocol -> fail "Unknown Protocol"

  let (direction, localPort, remoteIP, remotePort) = if localIP == srcIP
        then (PacketUp, srcPort, destIP, destPort)
        else (PacketDown, destPort, srcIP, srcPort)
      remoteHostname = Nothing
      processName    = show localPort

  return Traffic { .. }

--------------------------------------------------------------------------------
-- Pcap

getPacketContent :: PktHdr -> Ptr Word8 -> IO [Word8]
getPacketContent header = peekArray (fromIntegral (hdrCaptureLength header))

asCallback :: ([Word8] -> IO ()) -> Callback
asCallback f h p = getPacketContent h p >>= f

startPcap :: String -> Callback -> IO ()
startPcap interface f = void $ async $ do
  p <- openLive interface 120 True 500000
  setDirection p InOut
  loop p (-1) $ f

parseToChan :: InChan Traffic -> IP -> Callback
parseToChan chan localIP = asCallback $ \content ->
  case runGetOrFail (parsePacket localIP) $ pack content of
    Left _          -> return ()
    Right (_, _, t) -> writeChan chan t

--------------------------------------------------------------------------------
-- Network Info

word32ToIP :: Word32 -> IP
word32ToIP x = ( fromIntegral $ x
               , fromIntegral $ x `shiftR` 8
               , fromIntegral $ x `shiftR` 16
               , fromIntegral $ x `shiftR` 24 )

ipv4ToIP :: IPv4 -> IP
ipv4ToIP (IPv4 w) = word32ToIP w

getInterfaceMap :: IO (Map String IP)
getInterfaceMap = foldl' build empty <$> getNetworkInterfaces
  where
    build m i = insert (name i) (ipv4ToIP $ ipv4 i) m

--------------------------------------------------------------------------------
-- Interface

runPcap :: String -> IO (OutChan Traffic)
runPcap interface = do
  (inChan, outChan) <- newChan
  interfaces <- getInterfaceMap
  case lookup interface interfaces of
    Just ip -> do
      startPcap interface $ parseToChan inChan ip
      return outChan
    Nothing -> fail "No such device"
