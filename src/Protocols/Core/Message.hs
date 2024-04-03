{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Core.Message (PeerMessage (..), pserialize, pparse) where

import Protocols.Message (Message (..))
import Protocols.Serializable (Serializable (..))

import qualified Data.Word as Word
import Parser (Parser (eof, next))

import Control.Monad.State (StateT, evalStateT, runStateT)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter)
import Data.ByteString (ByteString, pack, unpack)
import Data.Function ((&))

data PeerMessage
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have
      -- | index of piece
      Word.Word32
  | BitField
      -- | Flags with pieces peer has
      [Bool]
  | Request
      -- | piece index
      Word.Word32
      -- | begin -- byte offset
      Word.Word32
      -- | length
      Word.Word32
  | Piece
      -- | piece index
      Word.Word32
      -- | begin -- byte offset
      Word.Word32
      -- | part of the piece
      ByteString
  | Cancel
      -- | piece index
      Word.Word32
      -- | begin -- byte offset
      Word.Word32
      -- | length
      Word.Word32
  deriving (Eq, Show)

liftWriter :: (MonadWriter [Message] m) => Writer [Word.Word8] () -> m ()
liftWriter writer = tell [Message $ pack $ execWriter writer]

liftParser :: (Parser Message m) => StateT [Word.Word8] Maybe PeerMessage -> m PeerMessage
liftParser parser = do
  (Message message) <- next
  case runStateT parser (unpack message) of
    Just (peerMessage, []) -> return peerMessage
    _ -> fail "No parse"

instance Serializable Message PeerMessage where
  serialize :: (MonadWriter [Message] w) => PeerMessage -> w ()
  serialize KeepAlive = liftWriter $ do
    return ()
  serialize Choke = liftWriter $ do
    tell [0]
  serialize Unchoke = liftWriter $ do
    tell [1]
  serialize Interested = liftWriter $ do
    tell [2]
  serialize NotInterested = liftWriter $ do
    tell [3]
  serialize (Have index) = liftWriter $ do
    tell [4]
    serialize index
  serialize (BitField flags) = liftWriter $ do
    tell [5]
    serialize flags
  serialize (Request index offset length) = liftWriter $ do
    tell [6]
    serialize index
    serialize offset
    serialize length
  serialize (Piece index offset subpiece) = liftWriter $ do
    tell [7]
    serialize index
    serialize offset
    tell (unpack subpiece)
  serialize (Cancel index offset length) = liftWriter $ do
    tell [8]
    serialize index
    serialize offset
    serialize length

  parse :: (Parser Message p) => p PeerMessage
  parse = liftParser $ do
    isEmpty <- eof
    if isEmpty
      then return KeepAlive
      else do
        messageType <- next

        case messageType of
          0 -> return Choke
          1 -> return Unchoke
          2 -> return Interested
          3 -> return NotInterested
          4 -> Have <$> parse
          5 -> BitField <$> parse
          6 -> Request <$> parse <*> parse <*> parse
          7 ->
            Piece <$> parse <*> parse <*> do
              let loop = do
                    finished <- eof
                    if finished
                      then return []
                      else (:) <$> next <*> loop
              pack <$> loop
          8 -> Cancel <$> parse <*> parse <*> parse
          _ -> fail "Incorrect message"

pserialize :: PeerMessage -> ByteString
pserialize (peerMessage :: PeerMessage) =
  peerMessage
    & serialize
    & execWriter
    & (\case [message :: Message] -> message; _ -> error "incorrect serialization")
    & serialize
    & execWriter
    & pack

pparse :: ByteString -> Maybe PeerMessage
pparse bstr =
  bstr
    & unpack
    & evalStateT parse
    & (\case Just (message :: Message) -> message; _ -> error "no parse")
    & (: [])
    & evalStateT parse
