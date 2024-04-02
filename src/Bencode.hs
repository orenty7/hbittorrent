{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bencode where

import Control.Applicative
import Control.Lens
import Control.Monad (forM_, replicateM)
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString as B
import Data.Functor
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Parser
import Prelude as P

data Bencode
  = BString ByteString
  | BInteger Integer
  | BList [Bencode]
  | BDict (M.Map ByteString Bencode)
  deriving (Eq, Show)

makePrisms ''Bencode

digit :: (Parser Word8 p) => p Integer
digit = asum $ P.map parser ['0' .. '9']
 where
  parser char = expectChar char $> int char
  int char = read [char]

unsigned :: (Parser Word8 p) => p Integer
unsigned = do
  digits <- some digit
  return $ P.foldl (\acc digit -> acc * 10 + digit) 0 digits

parseBInteger :: (Parser Word8 p) => p Integer
parseBInteger = do
  expectChar 'i'

  sign <-
    optional (expectChars "+-") <&> \case
      Just '-' -> -1
      _ -> 1

  value <- unsigned

  expectChar 'e'

  return $ sign * value

parseBString :: (Parser Word8 p) => p ByteString
parseBString = do
  len <- unsigned
  expectChar ':'
  string <- replicateM (fromInteger len) next

  return $ B.pack string

parseBList :: (Parser Word8 p) => p [Bencode]
parseBList = do
  expectChar 'l'
  list <- many parseBencode
  expectChar 'e'

  return list

parseBDict :: (Parser Word8 p) => p (Map ByteString Bencode)
parseBDict = do
  expectChar 'd'
  kvalues <- many $ do
    key <- parseBString
    value <- parseBencode
    return (key, value)

  expectChar 'e'

  return $ fromList kvalues

parseBencode :: (Parser Word8 p) => p Bencode
parseBencode =
  BInteger <$> parseBInteger
    <|> BString <$> parseBString
    <|> BList <$> parseBList
    <|> BDict <$> parseBDict

parse :: (MonadFail m) => ByteString -> m Bencode
parse bstr = case evalStateT parseBencode (B.unpack bstr) of
  [x] -> return x
  [] -> fail "No parse"
  _ -> fail "Bencode parsed ambiguously"

encode :: Bencode -> ByteString
encode = execWriter . encoder

encoder :: Bencode -> Writer ByteString ()
encoder (BInteger int) = do
  tell $ encodeUtf8 "i"
  tell $ encodeUtf8 $ T.pack $ show int
  tell $ encodeUtf8 "e"
encoder (BString bstr) = do
  tell $ encodeUtf8 $ T.pack $ show $ B.length bstr
  tell $ encodeUtf8 ":"
  tell bstr
encoder (BList list) = do
  tell $ encodeUtf8 "l"
  forM_ list $ \elem -> do
    encoder elem
  tell $ encodeUtf8 "e"
encoder (BDict map) = do
  tell $ encodeUtf8 "d"
  forM_ (toAscList map) $ \(key, value) -> do
    encoder (BString key)
    encoder value
  tell $ encodeUtf8 "e"

prepare :: String -> [Word8]
prepare = P.map (toEnum . fromEnum)