{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bencode where

import Control.Applicative
import Control.Lens
import Control.Monad (forM_, replicateM, when)
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString as B
import Data.Functor
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Prelude as P

type ParserT m a = StateT [Word8] m a

type Parser a = ParserT [] a

data Bencode
  = BString ByteString
  | BInteger Integer
  | BList [Bencode]
  | BDict (M.Map ByteString Bencode)
  deriving (Eq, Show)

makePrisms ''Bencode

peekByte :: Parser Word8
peekByte = StateT $ \case
  bytes@(x : _) -> return (x, bytes)
  _ -> fail "Unexpected EOF"

readByte :: Parser Word8
readByte = StateT $ \case
  (x : xs) -> return (x, xs)
  _ -> fail "Unexpected EOF"

peekChar :: Parser Char
peekChar = peekByte <&> (toEnum . fromEnum)

readChar :: Parser Char
readChar = readByte <&> (toEnum . fromEnum)

expectChar :: Char -> Parser ()
expectChar char = do
  ch <- readChar
  when (ch /= char) $ do
    fail "Incorrect char"

expectChars :: [Char] -> Parser Char
expectChars chars = asum $ P.map (\char -> expectChar char $> char) chars

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

parseBString :: Parser ByteString
parseBString = do
  len <- unsigned
  expectChar ':'
  string <- replicateM (fromInteger len) readByte

  return $ B.pack string

parseBList :: Parser [Bencode]
parseBList = do
  expectChar 'l'
  list <- many parseBencode
  expectChar 'e'

  return list

parseBDict :: Parser (Map ByteString Bencode)
parseBDict = do
  expectChar 'd'
  kvalues <- many $ do
    key <- parseBString
    value <- parseBencode
    return (key, value)

  expectChar 'e'

  return $ fromList kvalues

parseBencode :: Parser Bencode
parseBencode =
  BInteger <$> parseBInteger
    <|> BString <$> parseBString
    <|> BList <$> parseBList
    <|> BDict <$> parseBDict

parse :: ByteString -> Maybe Bencode
parse bstr = case evalStateT parseBencode (B.unpack bstr) of
  [x] -> Just x
  [] -> Nothing
  _ -> error "Bencode parsed ambiguously"

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