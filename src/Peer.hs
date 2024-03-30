{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Peer where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.State (evalStateT)
import Control.Monad.Writer
import Data.ByteString as B
import Data.Maybe
import Data.Word
import Network.Simple.TCP
import Parser
import Torrent
import Prelude as P

type Host = String

type Port = Word32

data Handshake = Handshake {_extentionFlags :: [Bool], _infoHash :: ByteString, _peerId :: ByteString} deriving (Eq, Show)

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
  | Piece {index :: Word32, begin :: Word32, piece :: ByteString}
  | Cancel {index :: Word32, begin :: Word32, length :: Word32}
  deriving (Show, Eq)

class Encodable a where
  encode :: a -> ByteString

instance Encodable ByteString where
  encode = id

encodeIntegral :: (Integral a) => Int -> a -> [Word8]
encodeIntegral n x = go n x []
  where
    go 0 x words = words
    go n x words = go (n - 1) (x `div` 256) $ convert (x `mod` 256) : words

    convert = toEnum . fromEnum

decodeIntegral :: (Integral a) => [Word8] -> a
decodeIntegral words =
  let go :: [Word8] -> Integer -> Integer
      go [] num = num
      go (w : ws) num = go ws (num * 256 + (toInteger w))
   in fromInteger $ go words 0

instance Encodable Word8 where
  encode word8 = B.pack $ encodeIntegral 1 word8

instance Encodable Word32 where
  encode word32 = B.pack $ encodeIntegral 4 word32

instance Encodable [Bool] where
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

instance Encodable Message where
  encode KeepAlive = B.empty
  encode Choke =
    B.pack [0]
  encode Unchoke =
    B.pack [1]
  encode Interested =
    B.pack [2]
  encode NotInterested =
    B.pack [3]
  encode (Have {..}) =
    B.pack [4] <> encode index
  encode (Bitfield {..}) =
    B.pack [5] <> encode flags
  encode (Request {..}) =
    B.pack [6] <> mconcat (P.map encode [index, begin, length])
  encode (Piece {..}) =
    B.pack [7] <> encode index <> encode begin <> piece
  encode (Cancel {..}) =
    B.pack [8] <> mconcat (P.map encode [index, begin, length])

requestPieceLength :: Word32
requestPieceLength = 2 ^ 14

bitunpack :: ByteString -> [Bool]
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

handshakeBytes :: ByteString
handshakeBytes = B.pack $ (19 :) $ B.unpack "BitTorrent protocol"

buildHandshake :: Handshake -> ByteString
buildHandshake handshake = execWriter $ do
  tell handshakeBytes
  tell $ encode $ (view extentionFlags handshake)
  tell $ handshake ^. Peer.infoHash
  tell $ handshake ^. Peer.peerId

decodeHandshake :: (Parser Word8 p) => p Handshake
decodeHandshake = do
  expectByte 19
  forM_ ("BitTorrent protocol" :: String) $ \char -> do
    expectChar char

  extentionFlags <- bitunpack <$> B.pack <$> replicateM 8 readByte
  infoHash <- B.pack <$> replicateM 20 readByte
  peerId <- B.pack <$> replicateM 20 readByte

  return $ Handshake extentionFlags infoHash peerId

buildMessage :: Message -> ByteString
buildMessage message =
  let encodedMessage = encode message
      convert :: Int -> Word32
      convert = fromInteger . toInteger
   in encode (convert $ B.length encodedMessage) <> encodedMessage

readWord32 :: (Parser Word8 p) => p Word32
readWord32 = decodeIntegral <$> replicateM 4 readByte

messageDecoder :: (MonadIO p, Parser Word8 p) => Torrent -> p Message
messageDecoder torrent = do
  messageLength <- readWord32
  if messageLength == 0
    then return KeepAlive
    else do
      messageType <- readByte
      case messageType of
        0 -> return Choke
        1 -> return Unchoke
        2 -> return Interested
        3 -> return NotInterested
        4 -> Have <$> readWord32
        5 -> do
          let pieceCount = B.length (torrent ^. pieces) `div` 20
          let bytesToRead = fromInteger $ toInteger (messageLength - 1)

          bytes <- replicateM bytesToRead readByte
          return $ Bitfield $ P.take pieceCount $ bitunpack (B.pack bytes)
        6 -> Request <$> readWord32 <*> readWord32 <*> readWord32
        7 -> Piece <$> readWord32 <*> readWord32 <*> (B.pack <$> replicateM bytesToRead readByte)
          where
            bytesToRead = fromInteger $ toInteger $ messageLength - 1 - 4 - 4
        8 -> Cancel <$> readWord32 <*> readWord32 <*> readWord32
        _ -> fail "Incorrect message type"
