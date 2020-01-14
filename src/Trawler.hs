{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Trawler where

import Packets

import Prelude hiding (lookup, foldr)
import Data.Map (Map, fromList, lookup, foldrWithKey, union, empty, insertWith, findWithDefault)
import Data.Maybe (mapMaybe)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, modifyMVar_, newMVar)
import Control.Monad (forever)
import Control.Monad.IO.Class

import Proc.Process
import Proc.Net

import           Streamly.Data.Fold as SF      ( foldMap )
import           Streamly.Prelude as S      ( repeatM, drain, mapM, chunksOf, intervalsOf, map, zipWithM)
import           Streamly (SerialT)
import Data.Map.Monoidal as MM (singleton, getMonoidalMap)
import Data.Monoid (Sum(..))
import Data.Function ((&))

mkInodeToProc :: IO (Map Int String)
mkInodeToProc =
  fromList . concatMap (\Proc{..} -> zip sockets (repeat name)) <$> procs

mkPortToProc :: IO (Map Int String)
mkPortToProc = do
  inodeToProc <- mkInodeToProc
  tcps <- getTcps
  return
    $ fromList
    $ mapMaybe
      (\x -> (Proc.Net.destPort x,) <$> (lookup (inode x) inodeToProc))
      tcps

runEvents :: IO ()
runEvents = do
  portToProc <- newEmptyMVar

  async $ forever $ do
    newPortToProc <- mkPortToProc
    putMVar portToProc newPortToProc
    threadDelay 10000

  return ()

  where
    mapNames mv m = do
      p2p <- readMVar mv
      return ()

procStream :: SerialT IO (Map Int String)
procStream = do
  portToProc <- liftIO $ newMVar empty
  -- refresh p2p
  liftIO $ async $ forever $ do
    newPortToProc <- mkPortToProc
    modifyMVar_ portToProc (return . union newPortToProc)
    threadDelay 1000000

  S.repeatM $ readMVar portToProc

data Direction = Up | Down

data Protocol = TCP | UDP

data Traffic
  = Traffic
  { size :: Int
  , direction :: Direction
  , protocol :: Protocol
  , localPort :: Int
  , remoteIP :: IP
  , remotePort :: Int
  , remoteHostName :: Maybe String
  , processName :: Maybe String
  }

run = do
   S.drain $ S.mapM print $ S.zipWithM (\x y -> return $ assoc x y) portMapStream procStream
  where
    assoc :: (Map Int Int) -> (Map Int String) -> (Map String Int)
    assoc sizeMap procMap = foldrWithKey (\k v m -> insertWith (+) (findWithDefault (show k) k procMap) v m) empty sizeMap
