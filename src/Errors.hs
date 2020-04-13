module Errors where

data TrawlerError
  = NoInterface String
  | NoInterfacePermission String
  deriving Show
