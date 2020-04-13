{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}


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
                                                , readBChan
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
import           Data.Map.Strict               as M
                                                ( Map
                                                , adjust
                                                , alter
                                                , toList
                                                , empty
                                                , insertWith
                                                , fromList
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
import           State
import           Draw

import Control.Exception (catch, SomeException)
import System.IO.Error (catchIOError)

--------------------------------------------------------------------------------
-- Setup

app :: App AppState AppStep Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

test = S.drain $ S.mapM (print) createTrafficStream

run :: IO ()
run = do
  chan <- newBChan 10
  _    <- forkIO $ S.drain $ S.mapM (writeBChan chan . AppStep) fullStream
  -- Build the inital appstate ourselves to capture any errors; Brick will
  -- swallow them once it takes over.
  AppStep traffic <- readBChan chan
  let start = mkStartState traffic
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app start
