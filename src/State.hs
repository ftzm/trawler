{-# LANGUAGE RecordWildCards #-}

module State where

import           Brick                          ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , halt
                                                )
import           Data.List                     as L
                                                ( foldl'
                                                )
import           Data.Map.Strict               as M
                                                ( Map
                                                , empty
                                                , alter
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Vector                   as VC
                                                ( Vector
                                                , take
                                                , cons
                                                , empty
                                                , fromList
                                                , filter
                                                , (!?)
                                                )
import qualified Graphics.Vty                  as V

import           Packets
import Errors

type Name = ()

data AppStep = AppStep [Traffic] | AppError TrawlerError deriving Show

data TrafficMap = TrafficMap PacketDirection (Map String Int)

data AppState
  = AppState
  { intervals :: Vector (Vector Traffic)
  , upMap :: TrafficMap
  , downMap :: TrafficMap
  }

startState :: AppState
startState = AppState VC.empty
                            (TrafficMap PacketUp M.empty)
                            (TrafficMap PacketDown M.empty)

mkStartState :: [Traffic] -> AppState
mkStartState = updateState startState

addToTrafficMap
  :: TrafficMap -- ^ the old map
  -> Vector Traffic -- ^ the new traffic
  -> TrafficMap -- ^ the new map
addToTrafficMap (TrafficMap d orig) =
  TrafficMap d
    . foldl' (\m Traffic {..} -> M.alter (f size) processName m) orig
    . VC.filter ((== d) . direction)
  where f diff v | Nothing <- v = Just diff
                 | Just x  <- v = Just $ x + diff

takeFromTrafficMap
  :: TrafficMap -- ^ the old map
  -> Vector Traffic -- ^ the old traffic
  -> TrafficMap -- ^ the new map
takeFromTrafficMap (TrafficMap d orig) =
  TrafficMap d
    . foldl' (\m Traffic {..} -> M.alter (f size) processName m) orig
    . VC.filter ((== d) . direction)
  where f diff v | Nothing <- v                = Nothing
                 | Just x  <- v , x - diff < 1 = Nothing
                 | Just x  <- v                = Just $ x - diff

updateTrafficMap
  :: Vector Traffic -- ^ the old traffic
  -> Vector Traffic -- ^ the new traffic
  -> TrafficMap -- ^ the old map
  -> TrafficMap -- ^ the new map
updateTrafficMap old new m = addToTrafficMap (takeFromTrafficMap m old) new

updateState :: AppState -> [Traffic] -> AppState
updateState s traffic = AppState
  { intervals = consInterval
  , upMap     = updateTrafficMap final (VC.fromList traffic) $ upMap s
  , downMap   = updateTrafficMap final (VC.fromList traffic) $ downMap s
  }
 where
  consInterval = VC.cons (VC.fromList traffic) $ VC.take 299 $ intervals s
  final        = fromMaybe VC.empty $ (intervals s) VC.!? 299

handleEvent
  :: AppState -> BrickEvent Name AppStep -> EventM Name (Next AppState)
handleEvent s (AppEvent (AppStep traffic)) = continue $ updateState s traffic
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent s (AppEvent (AppError _)) = halt s
handleEvent s _ = continue s
