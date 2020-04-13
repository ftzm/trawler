{-# LANGUAGE RecordWildCards #-}

module Draw where

import qualified Graphics.Vty                  as V
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
import           Brick.Widgets.Border           ( borderWithLabel )
import           Data.Ord                       ( Down(..) )
import           Control.Monad                  ( void
                                                , join
                                                )
import           Data.List                     as L
                                                ( foldl'
                                                , sortOn
                                                , filter
                                                , map
                                                )
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
import           Data.Map.Strict               as M
                                                ( Map
                                                , adjust
                                                , alter
                                                , toList
                                                , empty
                                                , insertWith
                                                , fromList
                                                )
import           Text.Printf
import           Packets
import           State

byteCountToHuman :: Int -> String
byteCountToHuman i = showAmount (floatAmount / unit) suffix
 where
  floatAmount    = fromIntegral i
  kb             = 1000.0
  mb             = 1000.0 ^ 2
  gb             = 1000.0 ^ 3
  (unit, suffix) = head $ L.filter
    ((<= floatAmount) . fst)
    [(gb, "GB"), (mb, "MB"), (kb, "kB"), (1, "B")]
  showAmount :: Float -> String -> String
  showAmount = printf "%.2f %s"

procTotals :: [Traffic] -> [(String, Int)]
procTotals =
  M.toList
    . foldl' (\m Traffic {..} -> insertWith (+) processName size m) M.empty

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
  getTotals (TrafficMap _ m) =
    vBox . L.map showProc . sortOn (Down . snd) . M.toList $ m
  inTotals  = getTotals $ downMap s
  outTotals = getTotals $ upMap s
  packets =
    vBox
      $ VC.toList
      $ VC.map (str . formatTraffic)
      $ VC.take 200
      $ join
      $ intervals s

theMap :: AttrMap
theMap = attrMap V.defAttr []
