{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Stream (fullStream, Traffic(..)) where

--------------------------------------------------------------------------------

import           Prelude                 hiding ( lookup
                                                , foldr
                                                )
import           Streamly.Data.Fold            as SF
                                                ( foldMap )
import           Streamly.Prelude              as S
                                                ( repeatM
                                                , intervalsOf
                                                , zipWithM
                                                )
import           Streamly                       ( SerialT )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( mapMaybe )
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                , union
                                                , empty
                                                )
import           Control.Concurrent.Chan.Unagi  ( readChan )
import           Control.Monad                  ( forever )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async )
import           Control.Concurrent.MVar        ( readMVar
                                                , modifyMVar_
                                                , newMVar
                                                )
import           Control.Monad.IO.Class (liftIO)

import Packets
import           Proc.Process
import           Proc.Net


--------------------------------------------------------------------------------

getInodeToProc :: IO (Map Int String)
getInodeToProc = mkMap <$> getProcs
  where mkMap = fromList . concatMap (\Proc {..} -> zip sockets (repeat name))

mkPortToProc :: IO (Map Int String)
mkPortToProc = assoc <$> getInodeToProc <*> getTCP
 where
  assoc i2p = fromList . mapMaybe
    (\TcpConn { destPort, inode } -> (destPort, ) <$> lookup inode i2p)

procStream :: SerialT IO (Map Int String)
procStream = do
  portToProc <- liftIO $ newMVar empty
  _ <- liftIO $ async $ forever $ do
    newPortToProc <- mkPortToProc
    modifyMVar_ portToProc (return . union newPortToProc)
    threadDelay 1000000
  S.repeatM $ readMVar portToProc

createTrafficStream :: SerialT IO Traffic
createTrafficStream = do
  outChan <- liftIO runPcap
  S.repeatM $ readChan outChan

trafficStream :: SerialT IO [Traffic]
trafficStream = createTrafficStream & S.intervalsOf 1 (SF.foldMap (: []))

fullStream :: SerialT IO [Traffic]
fullStream = S.zipWithM (\x y -> return $ assoc x y) trafficStream procStream
 where
  assoc :: [Traffic] -> Map Int String -> [Traffic]
  assoc traffic procMap =
    (\t -> t { processName = lookup (localPort t) procMap }) <$> traffic
