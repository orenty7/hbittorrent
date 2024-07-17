{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.Core (
  Parser,
  peek,
  next,
  eof,
  readChar,
  expectByte,
  expectChar,
  expectChars,
  runParser
) where

import Control.Applicative (asum)
import Control.Monad (when)
import Control.Monad.State (StateT (StateT, runStateT))
import Data.Functor (($>), (<&>))
import Data.Word (Word8)


type Parser = StateT [Word8] Maybe


peek :: Parser Word8
peek = StateT $ \case
  symbols@(symbol : _) -> return (symbol, symbols)
  _ -> fail "End of input"

next :: Parser Word8
next = StateT $ \case
  (symbol : rest) -> return (symbol, rest)
  _ -> fail "End of input"

eof :: Parser Bool
eof = StateT $ \input -> case input of
  [] -> return (True, input)
  _ -> return (False, input)

readChar :: Parser Char
readChar = next <&> toEnum . fromEnum

expectByte :: Word8 -> Parser ()
expectByte byte = do
  b <- next
  when (b /= byte) $ do
    fail "Incorrect byte"

expectChar :: Char -> Parser ()
expectChar char = do
  ch <- readChar
  when (ch /= char) $ do
    fail "Incorrect char"

expectChars :: [Char] -> Parser Char
expectChars chars = asum $ map (\char -> expectChar char $> char) chars

-- runParser :: IORef SocketParserState -> SocketParser t -> IO t

runParser :: MonadFail m => Parser t -> [Word8] -> m t 
runParser parser bytes = case runStateT parser bytes of
  Just (result, []) -> return result
  _ -> fail "Parsing failed" 