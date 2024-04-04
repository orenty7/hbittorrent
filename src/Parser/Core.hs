{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Core (
  Parser (..),
  peekByte,
  peekChar,
  readByte,
  readChar,
  expectByte,
  expectChar,
  expectChars,
) where

import Control.Applicative (Alternative, asum)
import Control.Monad (MonadPlus, when)
import Control.Monad.State (StateT (StateT))
import Data.Functor (($>), (<&>))
import Data.Word (Word8)

class (Monad m, MonadFail m, Alternative m) => Parser sym m | m -> sym where
  next :: m sym
  peek :: m sym
  eof :: m Bool

instance (Monad m, MonadFail m, MonadPlus m) => Parser sym (StateT [sym] m) where
  peek :: StateT [sym] m sym
  peek = StateT $ \case
    symbols@(symbol : _) -> return (symbol, symbols)
    _ -> fail "End of input"

  next :: StateT [sym] m sym
  next = StateT $ \case
    (symbol : rest) -> return (symbol, rest)
    _ -> fail "End of input"

  eof :: StateT [sym] m Bool
  eof = StateT $ \input -> case input of
    [] -> return (True, input)
    _ -> return (False, input)

peekByte :: (Parser Word8 p) => p Word8
peekByte = peek

readByte :: (Parser Word8 p) => p Word8
readByte = next

peekChar :: (Parser Word8 p) => p Char
peekChar = peek <&> toEnum . fromEnum

readChar :: (Parser Word8 p) => p Char
readChar = next <&> toEnum . fromEnum

expectByte :: (Parser Word8 p) => Word8 -> p ()
expectByte byte = do
  b <- readByte
  when (b /= byte) $ do
    fail "Incorrect byte"

expectChar :: (Parser Word8 p) => Char -> p ()
expectChar char = do
  ch <- readChar
  when (ch /= char) $ do
    fail "Incorrect char"

expectChars :: (Parser Word8 p) => [Char] -> p Char
expectChars chars = asum $ map (\char -> expectChar char $> char) chars
