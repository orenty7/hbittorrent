{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Torrent (
  Torrent (..),
  announce,
  name,
  pieceLength,
  pieces,
  fileLength,
  urlList,
  infoHash,
  --
  mkTorrent,
) where

import Bencode (Bencode (..), encode, _BDict, _BInteger, _BList, _BString)
import qualified Hash as H

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (decodeUtf8')
import Text.URI (URI, mkURI)
import Prelude as P

data Torrent = Torrent
  { _announce :: Maybe (Either URI [[URI]])
  , _name :: String
  , _pieceLength :: Integer
  , _pieces :: A.Array Int H.Hash
  , _fileLength :: Integer
  , _urlList :: Maybe [URI]
  , _infoHash :: H.Hash
  }
  deriving (Eq, Show)

makeLenses ''Torrent

maybeDecodeUtf8 :: B.ByteString -> Maybe T.Text
maybeDecodeUtf8 bstr = case decodeUtf8' bstr of
  Right text -> Just text
  _ -> Nothing


splitPieces :: B.ByteString -> [H.Hash]
splitPieces bstr 
  | B.null bstr = []
  | B.length bstr >= 20 = (H.mkHash $ B.take 20 bstr):splitPieces (B.drop 20 bstr)
  | otherwise = error "Incorrect hash length"

mkTorrent :: Bencode -> Maybe Torrent
mkTorrent bencode = do
  let announce = bencode ^? _BDict . (ix "announce") . _BString >>= maybeDecodeUtf8 >>= mkURI

  let rawAnnounceList = P.map (^.. each . _BString) (bencode ^.. _BDict . (ix "announce-list" . _BList . each . _BList))
  let announceList = P.map (mapMaybe (maybeDecodeUtf8 >=> mkURI)) rawAnnounceList

  info <- bencode ^? _BDict . ix "info" . _BDict
  name <- info ^? ix "name" . _BString >>= maybeDecodeUtf8
  pieceLength <- info ^? ix "piece length" . _BInteger
  pieces <- splitPieces <$> info ^? ix "pieces" . _BString
  fileLength <- info ^? ix "length" . _BInteger

  let urlList = bencode ^? _BDict . ix "url-list" . _BList

  let hash = H.hash $ encode (BDict info)

  let makeUri bstr = do
        text <- maybeDecodeUtf8 bstr
        if T.last text == '/'
          then mkURI (text <> name)
          else mkURI text

  let parsedUrlList = do
        list <- urlList
        let bstrings = list ^.. folded . _BString
        traverse makeUri bstrings

  return $
    Torrent
      (if P.null announceList then Left <$> announce else Just (Right announceList))
      (T.unpack name)
      pieceLength
      (A.listArray (0, length pieces - 1) pieces) 
      fileLength
      parsedUrlList
      hash