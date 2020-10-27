{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Stream
  ( fullStream
  , createTrafficStream
  , Traffic(..)
  , BrickDone(..)
  )
where

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
                                                , findWithDefault
                                                , lookup
                                                , union
                                                , empty
                                                )
import           Control.Concurrent.Chan.Unagi  ( readChan )
import           Control.Monad                  ( forever )
import           Control.Monad.Catch            ( throwM )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async )
import           Control.Concurrent.MVar        ( readMVar
                                                , modifyMVar_
                                                , newMVar
                                                )
import           Control.Monad.IO.Class         ( liftIO )

import           Packets
import           Errors
import           Proc.Process
import           Proc.Net


--------------------------------------------------------------------------------

-- |Convert a list of Proc records into a map from socket inode addresses
-- to executable names.
mkInodeToExe :: [Proc] -> Map Int String
mkInodeToExe = fromList . concatMap (\Proc {..} -> zip sockets (repeat name))

-- |Given a list of processes and a list of TCP connections, produce a map from
-- the local port of the connection to the name of the executable that created
-- the connection.
mkPortToExe :: [Proc] -> [SockConn] -> [SockConn]-> Map Int String
mkPortToExe procs tcp udp = fromList . mapMaybe mkPair $ tcp ++ udp
 where
  inodeToExe = mkInodeToExe procs
  mkPair SockConn { destPort, inode } = (destPort, ) <$> lookup inode inodeToExe

-- |A stream producing a continuously updated port-to-executable map.
procStream :: SerialT IO (Map Int String)
procStream = do
  portToExe <- liftIO $ newMVar empty
  _         <- liftIO $ async $ forever $ do
    newPortToExe <- mkPortToExe <$> getProcs <*> getTCP <*> getUDP
    modifyMVar_ portToExe (return . union newPortToExe)
    threadDelay 1000000
  S.repeatM $ readMVar portToExe

createTrafficStream :: String -> SerialT IO Traffic
createTrafficStream interface = do
  chanResult <- liftIO $ runPcap interface
  case chanResult of
    Right outChan -> S.repeatM $ readChan outChan
    Left e -> do
      liftIO $ putStrLn e
      throwM NoDevice

trafficStream :: String -> SerialT IO [Traffic]
trafficStream interface = createTrafficStream interface & S.intervalsOf 1 (SF.foldMap (: []))

fullStream :: String -> SerialT IO [Traffic]
fullStream interface = S.zipWithM (\x y -> return $ assoc x y) (trafficStream interface) procStream
 where
  assoc :: [Traffic] -> Map Int String -> [Traffic]
  assoc traffic procMap =
    (\t -> t { processName = findWithDefault (processName t) (localPort t) procMap }) <$> traffic
