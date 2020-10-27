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
                                                , BChan
                                                )
import           Control.Concurrent.Async       ( concurrently )
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
import Control.Exception ( catches, Handler(..) )
import           Control.Monad.Catch            ( throwM )
import Options.Applicative

import           Stream
import           State
import           Draw
import           Errors


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

runOptions :: IO Options
runOptions =
  execParser $ info (options <**> helper) $ fullDesc
  <> progDesc "Watch network traffic on INTERFACE"
  <> header "trawler - a network traffic monitor"

app :: App AppState AppStep Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

run :: IO ()
run = do
  opts <- runOptions
  chan <- newBChan 10
  flip catches [ Handler $ noDeviceHandler opts
               , Handler brickDoneHandler
               ]
    $ void $ concurrently
        (runStream opts chan)
        (runBrick chan)
  where
    runBrick :: BChan AppStep -> IO AppState
    runBrick chan = do
      let
        start = mkStartState []
        buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      customMain initialVty buildVty (Just chan) app start
      throwM BrickDone

    runStream :: Options -> BChan AppStep -> IO ()
    runStream opts chan = S.drain
      $ S.mapM (writeBChan chan . AppStep)
      $ fullStream
      $ interface opts

    noDeviceHandler :: Options -> NoDevice -> IO ()
    noDeviceHandler opts _ = putStrLn
      $ "Error: Device "
      <> (interface opts)
      <> " missing or forbidden."

    brickDoneHandler :: BrickDone -> IO ()
    brickDoneHandler _ = return ()
