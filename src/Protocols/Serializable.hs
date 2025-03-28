module Protocols.Serializable (Serializable, Serializer, serialize, parse, runSerializer) where

import Parser.Core (Parser, eof, next)

import qualified Data.Word as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B

import Control.Monad (replicateM, unless)
import Control.Monad.Writer (Writer, tell, execWriter)
import Data.Bits (Bits (testBit))

type Serializer = Writer B.Builder

runSerializer :: Serializer () -> B.ByteString
runSerializer = B.toStrict . B.toLazyByteString . execWriter

class Serializable a where
  serialize ::  a -> Serializer ()
  parse :: Parser a

instance Serializable W.Word8 where
  serialize :: W.Word8 -> Serializer ()
  serialize word8 = tell (B.word8 word8)

  parse :: Parser W.Word8
  parse = next

instance Serializable W.Word32 where
  serialize :: W.Word32 -> Serializer ()
  serialize word32 = tell (B.word32BE word32)

  parse :: Parser W.Word32
  parse = do
    let decode = fromInteger . foldl (\acc byte -> acc * 256 + toInteger byte) 0
    decode <$> replicateM 4 next

instance Serializable [Bool] where
  serialize :: [Bool] -> Serializer ()
  serialize flags = do
    let (subflags, flags') = splitAt 8 flags

    serialize $ foldl (\acc bit -> acc * 2 + if bit then 1 else 0) (0 :: W.Word8) subflags
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