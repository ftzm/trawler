{-# LANGUAGE RecordWildCards #-}

module Trawler where

--------------------------------------------------------------------------------

import           Prelude                 hiding ( lookup
                                                , foldr
                                                , take
                                                , filter
                                                , map
                                                )
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                )
import           Brick                          ( Widget
                                                , EventM
                                                , AttrMap
                                                , BrickEvent
                                                , App(..)
                                                , Next
                                                , customMain
                                                , neverShowCursor
                                                , continue
                                                , BrickEvent(..)
                                                , str
                                                , attrMap
                                                , halt
                                                , vBox
                                                , (<+>)
                                                , (<=>)
                                                , hLimitPercent
                                                , vLimitPercent
                                                , fill
                                                )
import           Brick.Widgets.List             ( list )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Streamly.Prelude              as S
                                                ( drain
                                                , mapM
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( void
                                                , join
                                                )
import qualified Graphics.Vty                  as V
import           Data.Map                      as M
                                                ( Map
                                                , adjustWithKey
                                                , toList
                                                , empty
                                                , insertWith
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.List                     as L
                                                ( foldl'
                                                , sortOn
                                                , filter
                                                , map
                                                )
import           Data.Ord                       ( Down(..) )
import           Data.Vector                   as VC
                                                ( Vector
                                                , take
                                                , cons
                                                , empty
                                                , fromList
                                                , (!?)
                                                , filter
                                                , concat
                                                , map
                                                , toList
                                                )
import           Text.Printf


import           Stream
import           Packets


--------------------------------------------------------------------------------
-- Setup

type Name = ()

data AppStep = AppStep [Traffic]

data AppState
  = AppState
  { intervals :: Vector (Vector Traffic)
  , upMap :: Map String Integer
  , downMap :: Map String Integer
  }

app :: App AppState AppStep Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

test = S.drain $ S.mapM (print) fullStream

run :: IO ()
run = do
  chan <- newBChan 10
  _    <- forkIO $ S.drain $ S.mapM (writeBChan chan . AppStep) fullStream
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let startState = AppState VC.empty M.empty M.empty
  void $ customMain initialVty buildVty (Just chan) app startState

--------------------------------------------------------------------------------
-- Handling events

data TrafficMap = TrafficMap PacketDirection (Map String Integer)

addToTrafficMap
  :: Vector Traffic -- ^ the new traffic
  -> TrafficMap -- ^ the old map
  -> TrafficMap -- ^ the new map
addToTrafficMap new (TrafficMap d orig) = TrafficMap d $ foldl'
    (\m Traffic {..} -> M.adjustWithKey
      (\_ v -> v - (fromIntegral size))
      (fromMaybe (show localPort) processName)
      m
    )
    orig
    new

takeFromTrafficMap
  :: Vector Traffic -- ^ the old traffic
  -> TrafficMap -- ^ the old map
  -> TrafficMap -- ^ the new map
takeFromTrafficMap old (TrafficMap d orig) = TrafficMap d $ foldl'
  (\m t -> M.adjustWithKey (\_ v -> v - (fromIntegral $ size t)) (getName t) m)
  orig
  $ VC.filter ((== PacketUp) . direction) old
  where getName t = fromMaybe (show $ localPort t) $ processName t

updateTrafficMap
  :: Vector Traffic -- ^ the old traffic
  -> Vector Traffic -- ^ the new traffic
  -> TrafficMap -- ^ the old map
  -> TrafficMap -- ^ the new map
updateTrafficMap old new (TrafficMap d m) = undefined

updateState :: AppState -> [Traffic] -> AppState
updateState s traffic = AppState
  { intervals = consInterval
  , upMap     = updateMap newUp finalUp $ upMap s
  , downMap   = updateMap newDown finalDown $ downMap s
  }
 where
  consInterval = VC.cons (VC.fromList traffic) $ VC.take 299 $ intervals s
  newUp        = L.filter ((== PacketUp) . direction) traffic
  newDown      = L.filter ((== PacketDown) . direction) traffic
  getFinal     = fromMaybe VC.empty $ (intervals s) VC.!? 299
  finalUp      = VC.filter ((== PacketUp) . direction) getFinal
  finalDown    = VC.filter ((== PacketDown) . direction) getFinal
  updateMap new old m = (addInterval (removeInterval m old) new)
  removeInterval orig = foldl'
    (\m Traffic {..} -> M.adjustWithKey
      (\_ v -> v - (fromIntegral size))
      (fromMaybe (show localPort) processName)
      m
    )
    orig
  addInterval :: Map String Integer -> [Traffic] -> Map String Integer
  addInterval orig = foldl'
    (\m Traffic {..} -> insertWith (+)
                                   (fromMaybe (show localPort) processName)
                                   (fromIntegral size)
                                   m
    )
    orig

handleEvent
  :: AppState -> BrickEvent Name AppStep -> EventM Name (Next AppState)
handleEvent s (AppEvent (AppStep traffic)) = continue $ updateState s traffic
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent s _ = continue s

--------------------------------------------------------------------------------
-- Drawing

byteCountToHuman :: Integer -> String
byteCountToHuman i = showAmount (floatAmount / unit) suffix
 where
  floatAmount    = fromIntegral i
  kb             = 1000.0
  mb             = 1000.0 ^ 2
  gb             = 1000.0 ^ 3
  (unit, suffix) = head $ L.filter
    ((< floatAmount) . fst)
    [(gb, "GB"), (mb, "MB"), (kb, "kB"), (1, "B")]
  showAmount :: Float -> String -> String
  showAmount = printf "%.2f %s"

procTotals :: [Traffic] -> [(String, Integer)]
procTotals =
  M.toList
    . foldl'
        (\m Traffic {..} -> insertWith
          (+)
          (fromMaybe (show localPort) processName)
          (fromIntegral size)
          m
        )
        M.empty

formatTraffic :: Traffic -> String
formatTraffic Traffic {..} =
  printf "%s %s:%s" (show direction) (show remoteIP) (show remotePort)

drawUI :: AppState -> [Widget Name]
drawUI s =
  [hLimitPercent 50 packetsBox <+> (vLimitPercent 50 inBox <=> outBox)]
 where
  inBox      = borderWithLabel (str "In") (inTotals <+> fill ' ')
  outBox     = borderWithLabel (str "Out") (outTotals <+> fill ' ')
  packetsBox = borderWithLabel (str "Packets") (packets <+> fill ' ')
  showProc (name, bytes) = str $ name ++ ": " ++ byteCountToHuman bytes
  inTotals  = vBox $ L.map showProc $ sortOn (Down . snd) $ M.toList $ downMap s
  outTotals = vBox $ L.map showProc $ sortOn (Down . snd) $ M.toList $ upMap s
  packets =
    vBox
      $ VC.toList
      $ VC.map (str . formatTraffic)
      $ VC.take 200
      $ join
      $ intervals s

theMap :: AttrMap
theMap = attrMap V.defAttr []
