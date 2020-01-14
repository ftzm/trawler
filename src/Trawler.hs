module Trawler where

--------------------------------------------------------------------------------

import           Prelude                 hiding ( lookup
                                                , foldr
                                                )
import           Streamly.Prelude              as S
                                                ( drain
                                                , mapM
                                                )

import Stream

--------------------------------------------------------------------------------


run :: IO ()
run = S.drain $ S.mapM print fullStream
