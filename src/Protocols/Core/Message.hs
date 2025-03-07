{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Core.Message (PeerMessage (..)) where

import Protocols.Serializable (Serializable (..), Serializer)

import Parser.Core (Parser, eof, next, rest)

import Control.Monad.Writer (MonadWriter (tell))

import Data.Word (Word32)
import Data.ByteString (ByteString, unpack)

data PeerMessage
  = KeepAlive
  | Choke
  | UnChoke
  | Interested
  | NotInterested
  | Have
      -- | index of piece
      Word32
  | BitField
      -- | Flags with pieces peer has
      [Bool]
  | Request
      -- | piece index
      Word32
      -- | begin -- byte offset
      Word32
      -- | length
      Word32
  | Piece
      -- | piece index
      Word32
      -- | begin -- byte offset
      Word32
      -- | part of the piece
      ByteString
  | Cancel
      -- | piece index
      Word32
      -- | begin -- byte offset
      Word32
      -- | length
      Word32
  deriving (Eq, Show)

instance Serializable PeerMessage where
  serialize :: PeerMessage -> Serializer ()
  serialize KeepAlive = do
    return ()
  serialize Choke = do
    tell [0]
  serialize UnChoke = do
    tell [1]
  serialize Interested = do
    tell [2]
  serialize NotInterested = do
    tell [3]
  serialize (Have index) = do
    tell [4]
    serialize index
  serialize (BitField flags) = do
    tell [5]
    serialize flags
  serialize (Request index offset length) = do
    tell [6]
    serialize index
    serialize offset
    serialize length
  serialize (Piece index offset subpiece) = do
    tell [7]
    serialize index
    serialize offset
    tell $ unpack subpiece
  serialize (Cancel index offset length) = do
    tell [8]
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
