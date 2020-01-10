{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Packets where

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
                                                , datalink
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
                                                , writeChan
                                                --, getChanContents
                                                , readChan
                                                )
import           Control.Concurrent.Async       ( async )
import           Control.Monad       ( void )
import           Streamly.Prelude as S      ( repeatM, drain, mapM, chunksOf, intervalsOf, map)
import           Streamly.Data.Fold as SF      ( foldMap )
import           Streamly (SerialT)
import Data.Map.Monoidal as MM (singleton, getMonoidalMap)
import Data.Monoid (Sum(..))
import Control.Monad.IO.Class
import Data.Function ((&))


--------------------------------------------------------------------------------
-- Types

data LinkHeader = LinkHeader
  deriving (Show)

type IP = (Int, Int, Int, Int)

data IPHeader
  = IPHeader
  { srcIP :: IP
  , destIP :: IP
  , size :: Int
  } deriving (Show)

data TCPHeader
  = TCPHeader
  { srcPort :: Int
  , destPort :: Int
  } deriving (Show)

type PacketHeaders = (LinkHeader, IPHeader, TCPHeader)

--------------------------------------------------------------------------------
-- Parse

getLinkHeader :: Get LinkHeader
getLinkHeader = LinkHeader <$ skip 14

getIP :: Get IP
getIP = (,,,) <$> g <*> g <*> g <*> g
  where g = fromIntegral <$> getWord8

getIPHeader :: Get IPHeader
getIPHeader = do
  skip 2
  size <- fromIntegral <$> getWord16be
  skip 12
  src  <- getIP
  dest <- getIP
  return $ IPHeader src dest size

getTCPHeader :: Get TCPHeader
getTCPHeader = do
  src  <- fromIntegral <$> getWord16be
  dest <- fromIntegral <$> getWord16be
  return $ TCPHeader src dest

parsePacket :: Get PacketHeaders
parsePacket = (,,) <$> getLinkHeader <*> getIPHeader <*> getTCPHeader

--------------------------------------------------------------------------------
-- Pcap

getPacketContent :: PktHdr -> Ptr Word8 -> IO [Word8]
getPacketContent header =
  peekArray (fromIntegral (hdrCaptureLength header))

asCallback :: ([Word8] -> IO ()) -> Callback
asCallback f h p = getPacketContent h p >>= f

startPcap :: Callback -> IO ()
startPcap f = void $ async $ do
  p <- openLive "wlp61s0" 65535 True 0
  setDirection p In
  setFilter p "tcp" True (fromIntegral @Int 0)
  void $ loop p (-1) f

--------------------------------------------------------------------------------
-- Stream

parseToChan :: (InChan PacketHeaders) -> Callback
parseToChan chan = asCallback $ \content ->
  case runGetOrFail parsePacket $ pack content of
    Left _        -> print "failure"
    Right (_,_,x) -> writeChan chan x

createPacketStream :: SerialT IO PacketHeaders
createPacketStream = do
  (inChan, outChan) <- liftIO newChan
  liftIO $ startPcap $ parseToChan inChan
  S.repeatM $ readChan outChan

runPackets :: IO ()
runPackets =
  let
    getPair (_,IPHeader{size},TCPHeader{destPort}) = (destPort, size)
    toSing (k, v) = MM.singleton k $ Sum v
  in
    createPacketStream
      & S.intervalsOf 5 (SF.foldMap $ toSing . getPair)
      & S.map (fmap getSum . getMonoidalMap)
      & S.mapM print
      & S.drain
