{-# LANGUAGE OverloadedStrings #-}

module Torrent (parse,
                Path,
                File (..),
                FS (..),                
                Torrent (..))  where


import Bencode
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

type ErrorMessage = String
type Result = Either ErrorMessage

type Path = [T.Text]
  
data File = File { path :: Path, length :: Integer } deriving (Eq, Show)
data FS = SingleFile File | Files { files :: [File], dirname :: T.Text } deriving (Eq, Show)

data Torrent = Torrent
  { announce :: T.Text
  , piece_length :: Integer
  , pieces :: [B.ByteString]
  , fs :: FS } deriving (Eq, Show)



lookup' :: (Show k, Ord k) => k -> M.Map k v -> Result v
lookup' key dict = case M.lookup key dict of
                    Just value -> return value
                    Nothing -> Left $ "Error, key " <> show key <> " not found"


chunk :: Int -> B.ByteString -> [B.ByteString]
chunk n ""  = []
chunk n str = (B.take n str):chunk n (B.drop n str)



i :: Bencode -> Result Integer
i (I int) = return int
i _ = Left "Error, expected int"

s :: Bencode -> Result B.ByteString
s (S str) = return str
s _ = Left "Error, expected string"

l :: Bencode -> Result [Bencode]
l (L list) = return list
l _ = Left "Error, expected list"

d :: Bencode -> Result Dict
d (D dict) = return dict
d _ = Left "Error, expected dict"

t :: B.ByteString -> Result T.Text
t str = case decodeUtf8' str of
          Right text -> return text
          Left err -> Left $ "Unicode Error: " <> show err


ls :: Bencode -> Result [B.ByteString]
ls raw = l raw >>= traverse s


file :: Dict -> Result File
file raw = do
  len  <- lookup' "length" raw >>= i
  path <- lookup' "path"   raw >>= ls >>= traverse t
  return $ File path len


parse :: B.ByteString -> Result Torrent
parse text = do
  (bencode, rest) <- decode text
  dict <- d bencode

  announce     <- lookup' "announce" dict >>= s >>= t
  info         <- lookup' "info"     dict >>= d 
  
  piece_length <- lookup' "piece length" info >>= i
  pieces       <- lookup' "pieces"       info >>= s >>= return.chunk 20

  fs <- case M.lookup "files" info of      
          Just list -> do
            files <-  (l list) >>= (traverse d) >>= (mapM file)                        
            dir <- lookup' "name" info >>= s >>= t

            return $ Files files dir
            
          Nothing -> do
            len <- lookup' "length" info >>= i
            path <- lookup' "name" info >>= s >>= t

            return $ SingleFile $ File [path] len
        
  return $ Torrent announce piece_length pieces fs
  
