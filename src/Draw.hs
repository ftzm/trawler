{-# LANGUAGE RecordWildCards #-}

module Draw where

import           Brick                          ( Widget
                                                , AttrMap
                                                , str
                                                , attrMap
                                                , vBox
                                                , (<+>)
                                                , (<=>)
                                                , hLimitPercent
                                                , vLimitPercent
                                                , fill
                                                )
import           Brick.Widgets.Border           ( borderWithLabel )
import           Control.Monad                  ( join)
import           Data.List                     as L
                                                ( foldl'
                                                , sortOn
                                                , filter
                                                , map
                                                , intersperse
                                                )
import           Data.Map.Strict               as M
                                                ( toList
                                                , empty
                                                , insertWith
                                                )
import           Data.Ord                       ( Down(..) )
import           Data.Vector                   as VC
                                                ( take
                                                , map
                                                , toList
                                                )
import qualified Graphics.Vty                  as V
import           Text.Printf (printf)
import           Packets
import           State

byteCountToHuman :: Int -> String
byteCountToHuman i = showAmount (floatAmount / unit) suffix
 where
  floatAmount    = fromIntegral i
  kb             = 1000.0
  mb             = 1000.0 ^ (2 :: Integer)
  gb             = 1000.0 ^ (3 :: Integer)
  (unit, suffix) = head $ L.filter
    ((<= floatAmount) . fst)
    [(gb, "GB"), (mb, "MB"), (kb, "kB"), (1, "B")]
  showAmount :: Float -> String -> String
  showAmount = printf "%.2f %s"

procTotals :: [Traffic] -> [(String, Int)]
procTotals =
  M.toList
    . foldl' (\m Traffic {..} -> insertWith (+) processName size m) M.empty

padLeft :: Int -> Char -> String -> String
padLeft size padder string = pad ++ string
  where
    buffer = max 0 $ size - (length string)
    pad = replicate buffer padder

formatIP :: IP -> String
formatIP (a,b,c,d) = mconcat $ intersperse "." $ L.map show [a,b,c,d]

formatTraffic :: Traffic -> String
formatTraffic Traffic {..} =
  mconcat $ [ padLeft 19 ' ' $ formatIP remoteIP
            , ":"
            , padLeft 5 ' ' $ show remotePort
            , arrow
            , show localPort
            ]
  where
    arrow = case direction of
      PacketUp -> " -> "
      PacketDown -> " <- "

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
