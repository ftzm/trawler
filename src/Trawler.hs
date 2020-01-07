{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Trawler where

import Prelude hiding (lookup)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (mapMaybe)

import Proc.Process
import Proc.Net


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
      (\x -> (destPort x,) <$> (lookup (inode x) inodeToProc))
      tcps
