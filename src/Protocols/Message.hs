{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocols.Message (MessageHeader (..), headerSize) where

import Protocols.Serializable (Serializable (parse, serialize))

import Data.Word (Word32)

headerSize :: Int
headerSize = 4

newtype MessageHeader = MessageHeader {size :: Word32} deriving(Eq, Show )
instance Serializable MessageHeader where
  parse = MessageHeader <$> parse
  serialize (MessageHeader size) = serialize size

-- newtype Message = Message [Word8] deriving (Eq, Show)

-- instance Serializable Message where
--   parse :: Parser.Parser Message
--   parse = do
--     (messageLength :: Word32) <- parse
--     let convert = fromInteger . toInteger

--     body <- replicateM (convert messageLength) Parser.next
--     return $ Message body

--   serialize ::  Message -> Serializer ()
--   serialize (Message message) = do
--     let (messageLength :: Word32) = convert $ length message
--     serialize messageLength
--     tell message
