module Protocols.Message (MessageHeader (..), headerSize) where

import Protocols.Serializable (Serializable (parse, serialize))

import Data.Word (Word32)

headerSize :: Int
headerSize = 4

newtype MessageHeader = MessageHeader {size :: Word32} deriving(Eq, Show )
instance Serializable MessageHeader where
  parse = MessageHeader <$> parse
  serialize (MessageHeader size) = serialize size

