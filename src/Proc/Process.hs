{-# LANGUAGE ScopedTypeVariables #-}
--TODO switch to type applications

module Proc.Process where

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
listDirFull fp = map (fp </>) <$> listDirectory fp

-- |perform an IO action, converting an IOError to Nothing.
maybeIO :: IO a -> IO (Maybe a)
maybeIO f = catch (Just <$> f) (\(_ :: IOError) -> return Nothing)

getSockets :: FilePath -> IO [Socket]
getSockets fp = do
  fdstats <- mapM getFileStatus =<< listDirFull (fp </> "fd")
  return $ map (fromIntegral . fileID) $ filter isSocket fdstats

getName :: FilePath -> IO String
getName fp = takeBaseName <$> getSymbolicLinkTarget (fp </> "exe")

mkProc :: FilePath -> IO (Maybe Proc)
mkProc fp = do
  n <- maybeIO $ getName fp
  s <- maybeIO $ getSockets fp
  return $ Proc <$> n <*> s

procs :: IO [Proc]
procs = do
  procdirs <- filter (all isDigit . takeBaseName) <$> listDirFull "/proc"
  catMaybes <$> mapM mkProc procdirs
