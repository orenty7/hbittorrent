{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Peer (Handshake (..), buildHandshake, decodeHandshake, handshakeSize) where

import Parser.Core (Parser, expectByte, expectChar, nextN)

import qualified Data.ByteString as B

import Control.Lens
import Control.Monad (forM_, replicateM)
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word8)

import Prelude as P

data Handshake = Handshake
  { _extentionFlags :: [Bool]
  , _infoHash :: B.ByteString
  , _peerId :: B.ByteString
  }
  deriving (Eq, Show)

makeLenses ''Handshake

data Message
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have {index :: Word32}
  | Bitfield {flags :: [Bool]}
  | Request {index :: Word32, begin :: Word32, length :: Word32}
  | Piece {index :: Word32, begin :: Word32, piece :: B.ByteString}
  | Cancel {index :: Word32, begin :: Word32, length :: Word32}
  deriving (Show, Eq)

class Encodable a where
  encode :: a -> B.ByteString

instance Encodable B.ByteString where
  encode :: B.ByteString -> B.ByteString
  encode = id

encodeIntegral :: (Integral a) => Int -> a -> [Word8]
encodeIntegral n x = go n x []
 where
  go 0 _ words = words
  go n x words = go (n - 1) (x `div` 256) $ convert (x `mod` 256) : words

  convert = toEnum . fromEnum

instance Encodable Word8 where
  encode :: Word8 -> B.ByteString
  encode word8 = B.pack $ encodeIntegral 1 word8

instance Encodable Word32 where
  encode :: Word32 -> B.ByteString
  encode word32 = B.pack $ encodeIntegral 4 word32

instance Encodable [Bool] where
  encode :: [Bool] -> B.ByteString
  encode flags = flip evalState flags $ do
    let pop = state $ \case
          [] -> (Nothing, [])
          (x : xs) -> (Just x, xs)

        isEmpty = gets P.null

        readWord8 = do
          bits <- P.map (fromMaybe False) <$> replicateM 8 pop
          return $ P.foldr (\bit acc -> 2 * acc + if bit then 1 else 0) 0 bits

        loop bytes = do
          stop <- isEmpty

          if stop
            then return $ P.reverse bytes
            else do
              byte <- readWord8
              loop (byte : bytes)

    B.pack <$> loop []

bitunpack :: B.ByteString -> [Bool]
bitunpack bstr =
  let packed :: Integer
      packed = P.foldr (\byte acc -> acc * 256 + toInteger byte) 0 (B.unpack bstr)

      go :: Int -> Integer -> [Bool] -> [Bool]
      go 0 _ flags = P.reverse flags
      go n x flags =
        if even x
          then go (n - 1) (x `div` 2) (False : flags)
          else go (n - 1) (x `div` 2) (True : flags)
   in go ((B.length bstr) * 8) packed []

handshakeBytes :: B.ByteString
handshakeBytes = B.pack $ (19 :) $ B.unpack "BitTorrent protocol"

handshakeSize :: Int
handshakeSize = 1 + 19 + 8 + 20 + 20

buildHandshake :: Handshake -> B.ByteString
buildHandshake handshake = execWriter $ do
  tell handshakeBytes
  tell $ encode $ view extentionFlags handshake
  tell $ handshake ^. Peer.infoHash
  tell $ handshake ^. Peer.peerId

decodeHandshake :: Parser Handshake
decodeHandshake = do
  expectByte 19
  forM_ ("BitTorrent protocol" :: String) $ \char -> do
    expectChar char

  extentionFlags <- bitunpack <$> nextN 8
  infoHash <- nextN 20
  peerId <- nextN 20

  return $ Handshake extentionFlags infoHash peerId
