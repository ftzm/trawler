{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proc.Process
  ( getProcs
  , Proc(..)
  , Socket
  )
where

--------------------------------------------------------------------------------

import           Control.Exception.Base         ( catch )
import           Data.Char                      ( isDigit )
import           Data.Maybe                     ( catMaybes )
import           System.Directory               ( listDirectory
                                                , getSymbolicLinkTarget
                                                )
import           System.FilePath.Posix          ( takeBaseName
                                                , (</>)
                                                )
import           System.Posix.Files             ( getFileStatus
                                                , isSocket
                                                , fileID
                                                )

--------------------------------------------------------------------------------
-- Types

type Socket = Int -- Inode

data Proc = Proc
          { name :: String
          , sockets :: [Socket]
          } deriving (Show)

--------------------------------------------------------------------------------

-- |List contents of a directory as the full paths.
listDirFull :: FilePath -> IO [FilePath]
listDirFull fp = map (fp </>) <$> {-# SCC callListDirectory #-} listDirectory fp

-- |perform an IO action, converting an IOError to Nothing.
maybeIO :: IO a -> IO (Maybe a)
maybeIO f = catch ({-# SCC setJust #-} Just <$> f) (\(_ :: IOError) -> {-# SCC setNothing #-} return Nothing)

getSockets
  :: FilePath -- ^ The directory of a process in /proc/
  -> IO [Socket] -- ^ a list of sockets owned by the process
getSockets fp = do
  fdstats <- {-# SCC fdstats #-} mapM ({-# SCC getFileStatus #-} getFileStatus) =<< {-# SCC callListDirFull #-} listDirFull (fp </> "fd")
  return $ {-# SCC narrowToSockets #-} map (fromIntegral . fileID) $ filter isSocket fdstats

getExeName
  :: FilePath -- ^ The directory of a process in /proc/
  -> IO String -- ^ the name of the executable of the process
getExeName fp = takeBaseName <$> getSymbolicLinkTarget (fp </> "exe")

-- |Build a Proc record from the directory of a process in /proc/
mkProc :: FilePath -> IO (Maybe Proc)
mkProc fp = do
  n <- maybeIO $ {-# SCC getExeName #-} getExeName fp
  s <- maybeIO $ {-# SCC getSockets #-} getSockets fp
  return $ Proc <$> n <*> s

getProcs :: IO [Proc]
getProcs = do
  procdirs <- filter (all isDigit . takeBaseName) <$> listDirFull "/proc"
  catMaybes <$> mapM mkProc procdirs
