{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bencode (
  Bencode (..),
  _BString,
  _BInteger,
  _BList,
  _BDict,
  --
  parse,
  encode,
  parser,
) where

import Parser.Core (Parser, next, expectChar, expectChars)

import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Applicative (asum, many, optional, some)
import Control.Lens (makePrisms)
import Control.Monad (forM_, replicateM)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Functor (($>), (<&>))

import Prelude as P

data Bencode
  = BString B.ByteString
  | BInteger Integer
  | BList [Bencode]
  | BDict (M.Map B.ByteString Bencode)
  deriving (Eq, Show)

makePrisms ''Bencode

digit :: Parser Integer
digit = asum $ P.map parser ['0' .. '9']
 where
  parser char = expectChar char $> int char
  int char = read [char]

unsigned :: Parser Integer
unsigned = do
  digits <- some digit
  return $ P.foldl (\acc digit -> acc * 10 + digit) 0 digits

parseBInteger :: Parser Integer
parseBInteger = do
  expectChar 'i'

  sign <-
    optional (expectChars "+-") <&> \case
      Just '-' -> -1
      _ -> 1

  value <- unsigned

  expectChar 'e'

  return $ sign * value

parseBString :: Parser B.ByteString
parseBString = do
  len <- unsigned
  expectChar ':'
  string <- replicateM (fromInteger len) next

  return $ B.pack string

parseBList :: Parser [Bencode]
parseBList = do
  expectChar 'l'
  list <- many parseBencode
  expectChar 'e'

  return list

parseBDict :: Parser (M.Map B.ByteString Bencode)
parseBDict = do
  expectChar 'd'
  kvalues <- many $ do
    key <- parseBString
    value <- parseBencode
    return (key, value)

  expectChar 'e'

  return $ M.fromList kvalues

parseBencode :: Parser Bencode
parseBencode =
  asum
    [ BInteger <$> parseBInteger
    , BString <$> parseBString
    , BList <$> parseBList
    , BDict <$> parseBDict
    ]

parser :: Parser Bencode
parser = parseBencode

parse :: (MonadFail m) => B.ByteString -> m Bencode
parse bstr = case evalStateT parseBencode (B.unpack bstr) of
  Just x -> return x
  Nothing -> fail "No parse"

encode :: Bencode -> B.ByteString
encode = B.toStrict . toLazyByteString . execWriter . encoder

bemit :: B.ByteString -> Writer Builder ()
bemit = tell . byteString

emit :: String -> Writer Builder ()
emit = bemit . fromString

encoder :: Bencode -> Writer Builder ()
encoder (BInteger int) = do
  emit "i"
  emit $ show int
  emit "e"
encoder (BString bstr) = do
  emit $ show $ B.length bstr
  emit ":"
  bemit bstr
encoder (BList list) = do
  emit "l"
  forM_ list $ \elem -> do
    encoder elem
  emit "e"
encoder (BDict map) = do
  emit "d"
  forM_ (M.toAscList map) $ \(key, value) -> do
    encoder (BString key)
    encoder value
  emit "e"