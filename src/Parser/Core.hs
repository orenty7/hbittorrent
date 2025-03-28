module Parser.Core (
  Parser,
  peek,
  next,
  nextN,
  rest,
  eof,
  readChar,
  expectByte,
  expectChar,
  expectChars,
  runParser
) where

import qualified Data.ByteString as B

import Control.Applicative (asum)
import Control.Monad (when)
import Control.Monad.State (StateT (StateT, runStateT))
import Data.Functor (($>), (<&>))
import Data.Word (Word8)

type Parser = StateT (B.ByteString, Int) Maybe


peek :: Parser Word8
peek = StateT $ \(bstr, index) -> do
  char <- bstr B.!? index
  return (char, (bstr, index))

next :: Parser Word8
next = StateT $ \(bstr, index) -> do
  char <- bstr B.!? index
  return (char, (bstr, index + 1))

nextN :: Int -> Parser B.ByteString
nextN n = StateT $ \(bstr, index) -> do
  if index + n > B.length bstr
    then Nothing
    else Just $ (B.take n (B.drop index bstr), (bstr, index + n))

rest :: Parser B.ByteString
rest = StateT $ \(bstr, index) -> return (B.drop index bstr, (bstr, B.length bstr))


eof :: Parser Bool
eof = StateT $ \(bstr, index) -> return (B.length bstr == index, (bstr, index))

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

runParser :: MonadFail m => Parser t -> B.ByteString -> m t 
runParser parser bstr = case runStateT parser (bstr, 0) of
  Nothing -> fail "Parsing failed" 
  Just (result, (bstr, pos)) -> 
    if B.length bstr == pos 
      then return result 
      else fail "Bytes left"