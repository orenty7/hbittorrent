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

module Peer (
  Handshake (..), 
  PeerState (..), 
  
  sendHandshake, 
  receiveHandshake, 
  sendMessage, 
  receiveMessage, 
  runPeer,
  communicate
) where

import Parser.Core (Parser, expectByte, expectChar, nextN, runParser)
import Protocols (MessageHeader (..), PeerMessage (..), Serializable (..), headerSize)
import qualified Hash as H 

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S


import Control.Concurrent
import Control.Lens
import Control.Monad (forM_, replicateM, when, forever)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word8)

import Prelude as P

data Handshake = Handshake
  { _extensionFlags :: [Bool]
  , _infoHash :: H.Hash
  , _peerId :: B.ByteString
  }
  deriving (Eq, Show)

makeLenses ''Handshake

data Message
  = KeepAlive
  | Choke
  | UnChoke
  | Interested
  | NotInterested
  | Have {index :: Word32}
  | BitField {flags :: [Bool]}
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

bitUnpack :: B.ByteString -> [Bool]
bitUnpack bstr =
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
  tell $ encode $ view extensionFlags handshake
  tell $ H.unHash $ handshake ^. Peer.infoHash
  tell $ handshake ^. Peer.peerId

decodeHandshake :: Parser Handshake
decodeHandshake = do
  expectByte 19
  forM_ ("BitTorrent protocol" :: String) $ \char -> do
    expectChar char

  extensionFlags <- bitUnpack <$> nextN 8
  infoHash <- H.mkHash <$> nextN 20
  peerId <- nextN 20

  return $ Handshake extensionFlags infoHash peerId

data PeerState = PeerState
  { _socket :: S.Socket
  }
  deriving (Show)

makeLenses ''PeerState

type Peer = ReaderT PeerState IO

recv :: Int -> Peer B.ByteString
recv 0 = return B.empty
recv n = do
  socket <- asks (view socket)
  let loop chunks 0 = return $ B.concat $ reverse $ chunks
      loop chunks n = do
        chunk <- liftIO $ S.recv socket n

        let loaded = B.length chunk

        when (loaded == 0) $ do
          fail "Socket closed"

        loop (chunk : chunks) (n - B.length chunk)

  loop [] n

send :: B.ByteString -> Peer ()
send bstr = do
  socket <- asks (view socket)
  liftIO $ S.send socket bstr
  return ()

sendHandshake :: Handshake -> Peer ()
sendHandshake handshake = do
  let bytes = buildHandshake handshake
  send bytes
  return ()

receiveHandshake :: Peer Handshake
receiveHandshake = do
  rawHandshake <- recv handshakeSize

  case runParser decodeHandshake rawHandshake of
    Nothing -> fail "Handshake failed"
    Just handshake -> return handshake

receiveMessage :: Peer PeerMessage
receiveMessage = do
  rawHeader <- recv headerSize

  case runParser parse rawHeader of
    Nothing -> fail "Header parsing failed"
    Just (MessageHeader size) -> do
      payload <- recv $ fromIntegral size

      runParser parse payload

sendMessage :: PeerMessage -> Peer ()
sendMessage message = do
  let runSerializer :: (Serializable a) => a -> B.ByteString
      runSerializer = B.pack . execWriter . serialize

      payload = runSerializer message
      header = runSerializer $ MessageHeader $ fromIntegral $ (B.length payload)

  send header
  send payload
  return ()

runPeer :: Peer a -> PeerState -> IO a
runPeer action config = runReaderT action config

-- program :: Peer ()
-- program = do
--   env <- ask
--   incoming <- liftIO $ STM.atomically STM.newTTChan

--   let receiver = forever $ do
--         msg <- receiveMessage
--         liftIO $ STM.atomically $ do
--           STM.writeTTChan incoming msg

--   liftIO $ forkIO $ runPeer receiver env

--   return ()

communicate :: S.Socket -> IO (STM.TChan PeerMessage, STM.TChan PeerMessage) 
communicate socket = do
  peerMessages <- STM.newTChanIO
  masterMessages <- STM.newTChanIO

  let runPeer action = runReaderT action (PeerState socket)

  forkIO $ runPeer $ forever $ do
    msg <- receiveMessage
    liftIO $ STM.atomically $ STM.writeTChan peerMessages msg
 
  forkIO $ runPeer $ forever $ do
    msg <- liftIO $ STM.atomically $ STM.readTChan masterMessages
    sendMessage msg

  return $ (peerMessages, masterMessages)