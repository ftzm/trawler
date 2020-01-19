{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}

module Proc.Net (getTCP, getUDP, SockConn(..), Port, IP) where

--------------------------------------------------------------------------------

import           Prelude                 hiding ( readFile
                                                , lines
                                                , takeWhile
                                                , take
                                                , concat
                                                , lookup
                                                )
import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.Word                      ( Word8 )
import           Data.Text                      ( lines
                                                , chunksOf
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.Attoparsec.Text           ( Parser
                                                , takeWhile
                                                , parseOnly
                                                , takeText
                                                , char
                                                , space
                                                , take
                                                , hexadecimal
                                                , decimal
                                                )
import           Data.Either                    ( rights )

--------------------------------------------------------------------------------
-- Types

type IP = (Word8, Word8, Word8, Word8)

type Port = Int

data SockConn
  = SockConn
  { destIP :: IP
  , destPort :: Port
  , inode :: Int
  } deriving (Show)

--------------------------------------------------------------------------------
-- TCP

parseHexIP :: Parser IP
parseHexIP = do
  hexParts <- chunksOf 2 <$> take 8
  case mapM (parseOnly hexadecimal) hexParts of
    Right (a : b : c : d : _) -> return (a, b, c, d)
    _                         -> fail "Unable to parse IP"

parseHostPort :: Parser (IP, Port)
parseHostPort = do
  ip   <- parseHexIP
  _    <- char ':'
  port <- hexadecimal @Int
  return (ip, port)

readNet :: Parser SockConn
readNet = do
  _ <- takeWhile isSpace >> takeWhile isDigit >> char ':' >> space
  (dh, dp) <- parseHostPort
  _        <- space
  _        <- parseHostPort
  _        <- space
  _        <- hexadecimal @Int
  _        <- space
  _        <- decimal @Int >> char ':' >> decimal @Int
  _        <- space
  _        <- decimal @Int >> char ':' >> hexadecimal @Int
  _        <- space
  _        <- decimal @Int
  _        <- takeWhile isSpace
  _        <- decimal @Int
  _        <- takeWhile isSpace
  _        <- decimal @Int
  _        <- space
  i        <- decimal
  _        <- takeText
  return $ SockConn dh dp i

getTCP :: IO [SockConn]
getTCP = do
  contents <- lines <$> readFile "/proc/net/tcp"
  return $ rights $ map (parseOnly readNet) contents

getUDP :: IO [SockConn]
getUDP = do
  contents <- lines <$> readFile "/proc/net/udp"
  return $ rights $ map (parseOnly readNet) contents
