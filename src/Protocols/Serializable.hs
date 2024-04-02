{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Serializable (Serializable, serialize, parse) where

import Parser (Parser (..))

import qualified Data.Word as Word

import Control.Monad (replicateM, unless)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Bits (Bits (testBit))

class Serializable sym a where
  serialize :: (MonadWriter [sym] w) => a -> w ()
  parse :: (Parser sym p) => p a

instance Serializable Word.Word8 Word.Word8 where
  serialize :: (MonadWriter [Word.Word8] w) => Word.Word8 -> w ()
  serialize word8 = tell [word8]

  parse :: (Parser Word.Word8 p) => p Word.Word8
  parse = next

instance Serializable Word.Word8 Word.Word32 where
  serialize :: (MonadWriter [Word.Word8] w) => Word.Word32 -> w ()
  serialize word32 = do
    let go 0 _ = return ()
        go n x = do
          go (n - 1) (x `div` 256)
          serialize (fromInteger (x `mod` 256) :: Word.Word8)

    go (4 :: Int) (toInteger word32)

  parse :: (Parser Word.Word8 p) => p Word.Word32
  parse = do
    let decode = fromInteger . foldl (\acc byte -> acc * 256 + toInteger byte) 0
    decode <$> replicateM 4 Parser.next

instance Serializable Word.Word8 [Bool] where
  serialize :: (MonadWriter [Word.Word8] w) => [Bool] -> w ()
  serialize flags = do
    let (subflags, flags') = splitAt 8 flags

    serialize $ foldl (\acc bit -> acc * 2 + if bit then 1 else 0) (0 :: Word.Word8) subflags
    unless (null flags') $ do
      serialize flags'

  parse :: (Parser Word.Word8 p) => p [Bool]
  parse = do
    finished <- eof
    if finished
      then return []
      else do
        word <- next
        let flags = map (testBit word) [0 .. 7]

        (flags <>) <$> parse