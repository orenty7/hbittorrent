{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Torrent where

import Bencode
import Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding
import Text.URI

data Torrent = Torrent
  { _announce :: URI,
    _name :: Text,
    _pieceLength :: Integer,
    _pieces :: [ByteString],
    _fileLength :: Integer,
    _urlList :: Maybe [URI],
    _infoHash :: ByteString
  }
  deriving (Eq, Show)

makeLenses ''Torrent

maybeDecodeUtf8 :: ByteString -> Maybe Text
maybeDecodeUtf8 bstr = case decodeUtf8' bstr of
  Right text -> Just text
  _ -> Nothing

splitPieces :: ByteString -> [ByteString]
splitPieces bstr
  | bstr == B.empty = []
  | otherwise = piece : splitPieces rest
  where
    (piece, rest) = B.splitAt 20 bstr

mkTorrent :: Bencode -> Maybe Torrent
mkTorrent bencode = do
  announce <- bencode ^? _BDict . ix "announce" . _BString >>= maybeDecodeUtf8 >>= mkURI

  info <- bencode ^? _BDict . ix "info" . _BDict
  name <- info ^? ix "name" . _BString >>= maybeDecodeUtf8
  pieceLength <- info ^? ix "piece length" . _BInteger
  pieces <- splitPieces <$> info ^? ix "pieces" . _BString
  fileLength <- info ^? ix "length" . _BInteger

  let urlList = bencode ^? _BDict . ix "url-list" . _BList

  let hash = SHA1.hash $ encode (BDict info)

  let makeUri bstr = do
        text <- maybeDecodeUtf8 bstr
        if T.last text == '/'
          then mkURI (text <> name)
          else mkURI text

  let parsedUrlList = do
        list <- urlList
        let bstrings = list ^.. folded . _BString
        traverse makeUri bstrings

  return $ Torrent announce name pieceLength pieces fileLength parsedUrlList hash