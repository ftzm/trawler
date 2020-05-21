{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trawler where

--------------------------------------------------------------------------------

import           Brick                          ( App(..)
                                                , customMain
                                                , neverShowCursor
                                                )
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                , readBChan
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( void )
import qualified Graphics.Vty                  as V
import           Prelude                 hiding ( lookup
                                                , foldr
                                                , take
                                                , filter
                                                , map
                                                )
import           Streamly.Prelude              as S
                                                ( drain
                                                , mapM
                                                )
import Options.Applicative

import           Stream
import           State
import           Draw


--------------------------------------------------------------------------------
-- Config

data Options = Options
  { interface :: String
  }

options :: Parser Options
options = Options
  <$> strOption
     ( long "interface"
     <> help "The network interface to watch"
     )

app :: App AppState AppStep Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

run :: IO ()
run = do
  opts <- execParser $ info (options <**> helper) $
          fullDesc
          <> progDesc "Watch network traffic on INTERFACE"
          <> header "trawler - a network traffic monitor"

  -- Start streaming packet data into BChan for Brick
  chan <- newBChan 10
  forkIO
    $ S.drain
    $ S.mapM (writeBChan chan . AppStep)
    $ fullStream
    $ interface opts

  -- Build a real inital appstate ourselves to capture any errors;
  -- Brick will swallow them once it takes over.
  AppStep traffic <- readBChan chan
  let start = mkStartState traffic
      buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  -- Start Brick interface
  void $ customMain initialVty buildVty (Just chan) app start
