{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocols.Message (Message (..)) where

import Parser.Core qualified as Parser
import Protocols.Serializable (Serializable (parse, serialize))
import Utils (convert)

import Control.Monad (replicateM)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Word (Word32, Word8)

newtype Message = Message [Word8] deriving (Eq, Show)

instance Serializable Word8 Message where
  parse :: (Parser.Parser Word8 p) => p Message
  parse = do
    (messageLength :: Word32) <- parse
    let convert = fromInteger . toInteger

    body <- replicateM (convert messageLength) Parser.next
    return $ Message body

  serialize :: (MonadWriter [Word8] w) => Message -> w ()
  serialize (Message message) = do
    let (messageLength :: Word32) = convert $ length message
    serialize messageLength
    tell message
