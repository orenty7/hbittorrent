{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Serializable (Serializable, Serializer, serialize, parse) where

import Parser.Core (Parser, eof, next)

import qualified Data.Word as Word

import Control.Monad (replicateM, unless)
import Control.Monad.Writer (Writer, tell)
import Data.Bits (Bits (testBit))

type Serializer = Writer [Word.Word8]


class Serializable a where
  serialize ::  a -> Serializer ()
  parse :: Parser a

instance Serializable Word.Word8 where
  serialize :: Word.Word8 -> Serializer ()
  serialize word8 = tell [word8]

  parse :: Parser Word.Word8
  parse = next

instance Serializable Word.Word32 where
  serialize :: Word.Word32 -> Serializer ()
  serialize word32 = do
    let go 0 _ = return ()
        go n x = do
          go (n - 1) (x `div` 256)
          serialize (fromInteger (x `mod` 256) :: Word.Word8)

    go (4 :: Int) (toInteger word32)

  parse :: Parser Word.Word32
  parse = do
    let decode = fromInteger . foldl (\acc byte -> acc * 256 + toInteger byte) 0
    decode <$> replicateM 4 next

instance Serializable [Bool] where
  serialize :: [Bool] -> Serializer ()
  serialize flags = do
    let (subflags, flags') = splitAt 8 flags

    serialize $ foldl (\acc bit -> acc * 2 + if bit then 1 else 0) (0 :: Word.Word8) subflags
    unless (null flags') $ do
      serialize flags'

  parse :: Parser [Bool]
  parse = do
    finished <- eof
    if finished
      then return []
      else do
        word <- next
        let flags = map (testBit word) [0 .. 7]

        (flags <>) <$> parse