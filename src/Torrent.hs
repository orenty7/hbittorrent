{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Torrent where

import Bencode
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding
import Text.URI

data Torrent = Torrent
  { _announce :: URI,
    _pieceLength :: Integer,
    _pieces :: [ByteString],
    _fileLength :: Integer,
    _urlList :: [URI],
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
  info <- bencode ^? _BDict . (ix "info") . _BDict
  name <- (info ^? (ix "name") . _BString) >>= maybeDecodeUtf8
  pieceLength <- info ^? (ix "piece length") . _BInteger
  pieces <- splitPieces <$> info ^? (ix "pieces") . _BString
  fileLength <- info ^? (ix "length") . _BInteger

  let urlList = bencode ^.. _BDict . (ix "url-list") . _BList . folded . _BString

  let hash = SHA1.hash $ encode (BDict info)

  let makeUri bstr = do
        text <- maybeDecodeUtf8 bstr
        if T.last text == '/'
          then mkURI (text <> name)
          else mkURI text

  parsedUrlList <- traverse makeUri urlList

  return $ Torrent emptyURI pieceLength pieces fileLength parsedUrlList hash