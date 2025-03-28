{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Core.Message (PeerMessage (..)) where

import Protocols.Serializable (Serializable (..), Serializer)

import Parser.Core (Parser, eof, next, rest)

import Data.Word (Word8, Word32)
import Data.ByteString (ByteString, unpack)

data PeerMessage
  = KeepAlive
  | Choke
  | UnChoke
  | Interested
  | NotInterested
  | Have {index :: Word32}
  | BitField {flags :: [Bool]}
  | Request {index :: Word32, begin :: Word32, length :: Word32}
  | Piece {index :: Word32, begin :: Word32, piece :: ByteString}
  | Cancel {index :: Word32, begin :: Word32, length :: Word32}
  deriving (Show, Eq)

instance Serializable PeerMessage where
  serialize :: PeerMessage -> Serializer ()
  serialize KeepAlive = do
    return ()
  serialize Choke = do
    serialize (0 :: Word8)
  serialize UnChoke = do
    serialize (1 :: Word8)
  serialize Interested = do
    serialize (2 :: Word8)
  serialize NotInterested = do
    serialize (3 :: Word8)
  serialize (Have index) = do
    serialize (4 :: Word8)
    serialize index
  serialize (BitField flags) = do
    serialize (5 :: Word8)
    serialize flags
  serialize (Request index offset length) = do
    serialize (6 :: Word8)
    serialize index
    serialize offset
    serialize length
  serialize (Piece index offset subpiece) = do
    serialize (7 :: Word8)
    serialize index
    serialize offset
    mapM_ serialize (unpack subpiece)
  serialize (Cancel index offset length) = do
    serialize (8 :: Word8)
    serialize index
    serialize offset
    serialize length

  parse :: Parser PeerMessage
  parse = do
    isEmpty <- eof
    if isEmpty
      then return KeepAlive
      else do
        messageType <- next

        case messageType of
          0 -> return Choke
          1 -> return UnChoke
          2 -> return Interested
          3 -> return NotInterested
          4 -> Have <$> parse
          5 -> BitField <$> parse
          6 -> Request <$> parse <*> parse <*> parse
          7 -> Piece <$> parse <*> parse <*> rest
          8 -> Cancel <$> parse <*> parse <*> parse
          _ -> fail "Incorrect message"
