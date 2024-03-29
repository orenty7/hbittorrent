{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Control.Applicative
import Control.Monad (MonadPlus, when)
import Control.Monad.State
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Prelude as P

class (Monad m, MonadFail m, Alternative m) => Parser sym m | m -> sym where
  next :: m sym
  peek :: m sym

instance (Monad m, MonadFail m, MonadPlus m) => Parser sym (StateT [sym] m) where
  peek = StateT $ \case
    symbols@(symbol : _) -> return (symbol, symbols)
    _ -> fail "End of input"

  next = StateT $ \case
    (symbol : rest) -> return (symbol, rest)
    _ -> fail "End of input"

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
expectChars chars = asum $ P.map (\char -> expectChar char $> char) chars

orFail :: (MonadFail m) => Maybe a -> String -> m a
orFail value message = case value of
  Just x -> return x
  Nothing -> fail message

thenFail :: (MonadFail m) => Maybe a -> (a -> String) -> m ()
thenFail value createMessage = case value of
  Just x -> fail $ createMessage x
  Nothing -> return ()
