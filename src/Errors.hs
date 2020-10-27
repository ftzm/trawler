module Errors where

import           Control.Monad.Catch            ( Exception )

data NoDevice = NoDevice deriving Show
instance Exception NoDevice

data BrickDone = BrickDone deriving Show
instance Exception BrickDone
