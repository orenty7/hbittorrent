{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocols.Message (Message (..)) where

import Protocols.Serializable (Serializable (parse, serialize))

import qualified Data.ByteString as BS
import qualified Data.Word as Word
import qualified Parser

import Control.Monad (replicateM)
import Control.Monad.Writer (MonadWriter (tell))

newtype Message = Message BS.ByteString deriving (Eq, Show)

instance Serializable Word.Word8 Message where
  parse :: (Parser.Parser Word.Word8 p) => p Message
  parse = do
    (messageLength :: Word.Word32) <- parse
    let convert = fromInteger . toInteger

    body <- BS.pack <$> replicateM (convert messageLength) Parser.next
    return $ Message body

  serialize :: (MonadWriter [Word.Word8] w) => Message -> w ()
  serialize (Message message) = do
    let (messageLength :: Word.Word32) = fromInteger $ toInteger $ BS.length message
    serialize messageLength
    tell (BS.unpack message)
