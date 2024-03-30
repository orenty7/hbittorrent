{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Torrent where

import Bencode
import Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding
import Text.URI
import qualified Data.Maybe
import Control.Monad
import Prelude as P

data Torrent = Torrent
  { _announce :: Maybe (Either URI [[URI]]),
    _name :: Text,
    _pieceLength :: Integer,
    _pieces :: ByteString,
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



piece :: Int -> Lens' Torrent ByteString
piece index = let offset = index * 20 in
    lens ((B.take 20 . B.drop offset) . (view pieces))
         (\torrent newhash -> (set pieces (B.take offset (torrent^.pieces) <> newhash <> B.drop (offset + 20) (torrent^.pieces)) torrent)) 

mkTorrent :: Bencode -> Maybe Torrent
mkTorrent bencode = do
  let announce = bencode ^?_BDict.(ix "announce")._BString >>= maybeDecodeUtf8 >>= mkURI
  
  let rawAnnounceList = P.map (^..each._BString) (bencode^.._BDict.(ix "announce-list"._BList.each._BList))
  let announceList = P.map (Data.Maybe.mapMaybe (maybeDecodeUtf8 >=> mkURI)) rawAnnounceList

  info <- bencode ^? _BDict . ix "info" . _BDict
  name <- info ^? ix "name" . _BString >>= maybeDecodeUtf8
  pieceLength <- info ^? ix "piece length" . _BInteger
  pieces <- info ^? ix "pieces" . _BString
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
  
  return $ Torrent (if P.null announceList then Left <$> announce else Just (Right announceList)) name pieceLength pieces fileLength parsedUrlList hash