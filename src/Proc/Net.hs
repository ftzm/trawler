{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Proc.Net where

import Prelude hiding (readFile, lines, takeWhile, take, concat, lookup)

import Data.Char (isDigit, isSpace)
import Data.Text (lines, chunksOf)
import Data.Text.IO (readFile)
import Data.Attoparsec.Text (Parser, takeWhile, parseOnly, takeText, char, space, take, hexadecimal, decimal)
import Data.Either (rights)

--------------------------------------------------------------------------------
-- General

type IP = (Int, Int, Int, Int)

type Port = Int

--------------------------------------------------------------------------------
-- TCP

data TcpConn = TcpConn
          { destIP :: IP
          , destPort :: Port
          , inode :: Int
          } deriving (Show)

parseHexIP :: Parser (Int, Int, Int, Int)
parseHexIP = do
  hexParts <- chunksOf 2 <$> take 8
  case mapM (parseOnly hexadecimal) hexParts of
    Right (a:b:c:d:_) -> return (a,b,c,d)
    _ -> fail "Unable to parse IP"

parseHostPort :: Parser (IP, Port)
parseHostPort = do
  ip <- parseHexIP
  _ <- char ':'
  port <- hexadecimal @Int
  return (ip, port)

readTcp :: Parser TcpConn
readTcp = do
  _ <- takeWhile isSpace >> takeWhile isDigit >> char ':' >> space
  (destHost, destPort) <- parseHostPort
  _ <- space
  _ <- parseHostPort
  _ <- space
  _ <- hexadecimal @Int
  _ <- space
  _ <- decimal @Int
  _ <- char ':'
  _ <- decimal @Int
  _ <- space
  _ <- decimal @Int
  _ <- char ':'
  _ <- hexadecimal @Int
  _ <- space
  _ <- decimal @Int
  _ <- takeWhile isSpace
  _ <- decimal @Int
  _ <- takeWhile isSpace
  _ <- decimal @Int
  _ <- space
  inode <- decimal
  _ <- takeText
  return $ TcpConn destHost destPort inode

getTcps :: IO [TcpConn]
getTcps = do
  contents <- lines <$> readFile "/proc/net/tcp"
  return $ rights $ map (parseOnly readTcp) contents
